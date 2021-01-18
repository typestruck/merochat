--missing tests
module Server.Profile.Action where

import Prelude
import Server.Types hiding (BenderAction(..))
import Shared.Types

import Data.Array as DA
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.Bender as SB
import Server.File as SF
import Server.Ok (ok)
import Server.Profile.Database as SPD
import Server.Response as SR
import Shared.DateTime as SDT
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, maxLanguages, maxTags, nameMaxCharacters, tagMaxCharacters)
import Shared.Unsafe as SU

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

      updatedAvatar <- DM.maybe (pure Nothing) (map Just <<< SF.saveBase64File) avatar
      SPD.saveProfile {
            user: profileUser { id = loggedUserID },
            avatar: updatedAvatar,
            languages,
            tags
      }
      pure ok
      where isNull = DS.null <<< DS.trim
            anyFieldIsTooBig = DS.length name > nameMaxCharacters || DS.length headline > headlineMaxCharacters || DS.length description > descriptionMaxCharacters || DA.any ((_ > tagMaxCharacters) <<< DS.length) tags