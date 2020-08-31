module Server.Profile.Action where

import Prelude
import Server.Types hiding (BenderAction(..))
import Shared.Types

import Data.Array as DA
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.Bender as SB
import Shared.Options.File (allowedMediaTypes)
import Server.Ok (ok)
import Server.Profile.Database as SPD
import Server.Response as SR
import Shared.Options.File (maxImageSize, maxImageSizeKB)
import Shared.Unsafe as SU

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for avatar is " <> maxImageSizeKB

generate :: Generate -> ServerEffect String
generate =
      case _ of
            Name -> SB.generateName
            Headline -> SB.generateHeadline
            Description -> SB.generateDescription

saveProfile :: PrimaryKey -> ProfileUser -> ServerEffect Ok
saveProfile loggedUserID profileUser@{ name, headline, description, avatar, languages, tags } = do
      when (isNull name || isNull headline || isNull description) $ SR.throwBadRequest "Missing required info fields"

      updatedAvatar <- base64From $ map (DS.split (Pattern ",")) avatar
      SPD.saveProfile {
            user: profileUser { id = loggedUserID },
            avatar: updatedAvatar,
            languages,
            tags
      }
      pure ok
      where isNull = DS.null <<< DS.trim
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
