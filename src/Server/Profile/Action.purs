module Server.Profile.Action where

import Prelude
import Server.Types hiding (BenderAction(..))
import Shared.Types

import Data.Array as DA
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.Bender as SB
import Server.Ok (ok)
import Server.Profile.Database as SPD
import Server.Response (throwBadRequest)
import Server.Response as SR
import Shared.DateTime as SDT
import Shared.Options.File (allowedMediaTypes, maxImageSize, maxImageSizeKB)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, maxLanguages, maxTags, nameMaxCharacters, tagMaxCharacters)
import Shared.Unsafe as SU

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for profile picture is " <> maxImageSizeKB

missingRequiredFieldsMessage :: String
missingRequiredFieldsMessage = "Name, headline and description are mandatory"

tooManyTagsMessage :: String
tooManyTagsMessage = "Number of tags larger than " <> show maxTags <> " limit"

tooManyLanguagesMessage :: String
tooManyLanguagesMessage = "Number of languages larger than " <> show maxLanguages <> " limit"

tooYoungMessage :: String
tooYoungMessage = "You must be over 13 years old in order to use MelanChat"

fieldTooBigMessage :: String
fieldTooBigMessage = "Field exceeded max value"

generate :: Generate -> ServerEffect String
generate =
      case _ of
            Name -> SB.generateName
            Headline -> SB.generateHeadline
            Description -> SB.generateDescription

saveProfile :: PrimaryKey -> ProfileUser -> ServerEffect Ok
saveProfile loggedUserID profileUser@{ name, age, headline, description, avatar, languages, tags } = do
      when (isNull name || isNull headline || isNull description) $ SR.throwBadRequest missingRequiredFieldsMessage
      when anyFieldIsTooBig $ SR.throwBadRequest fieldTooBigMessage
      when (DA.length tags > maxTags) $ SR.throwBadRequest tooManyTagsMessage
      when (DA.length languages > maxLanguages) $ SR.throwBadRequest tooManyLanguagesMessage
      thirteen <- Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap age > thirteen) $ SR.throwBadRequest tooYoungMessage

      updatedAvatar <- base64From $ map (DS.split (Pattern ",")) avatar
      SPD.saveProfile {
            user: profileUser { id = loggedUserID },
            avatar: updatedAvatar,
            languages,
            tags
      }
      pure ok
      where isNull = DS.null <<< DS.trim
            anyFieldIsTooBig = DS.length name > nameMaxCharacters || DS.length headline > headlineMaxCharacters || DS.length description > descriptionMaxCharacters || DA.any ((_ > tagMaxCharacters) <<< DS.length) tags
            base64From =
                  case _ of
                        Just [mediaType, base64] -> do
                              if FD.any (_ == mediaType) $ DH.keys allowedMediaTypes then do
                                    buffer <- R.liftEffect $ NB.fromString base64 Base64
                                    bufferSize <- R.liftEffect $ NB.size buffer
                                    if bufferSize > maxImageSize then
                                          SR.throwBadRequest imageTooBigMessage
                                     else do
                                          uuid <- R.liftEffect (DU.toString <$> DU.genUUID)
                                          let fileName = uuid <> SU.fromJust (DH.lookup mediaType allowedMediaTypes)
                                          R.liftEffect $ NFS.writeFile ("src/Client/media/upload/" <> fileName) buffer

                                          pure $ Just fileName
                               else
                                    SR.throwBadRequest invalidImageMessage
                        Just [savedFile] -> pure <<< DA.last $ DS.split (Pattern "/") savedFile
                        _ -> pure Nothing
