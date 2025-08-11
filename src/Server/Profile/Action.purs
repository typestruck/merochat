module Server.Profile.Action where

import Prelude
import Shared.Options.Profile

import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Data.Tuple as DT
import Debug (spy)
import Run as R
import Server.Database as SD
import Server.Database.CompleteProfiles (ProfileColumn(..))
import Server.Database.Privileges as SDP
import Server.Effect (ServerEffect)
import Server.File as SF
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Types (Payload)
import Server.Response as SR
import Server.Sanitize as SS
import Server.ThreeK as ST
import Shared.DateTime (DateWrapper)
import Shared.DateTime as SDT
import Shared.Privilege (Privilege(..))
import Shared.Profile.Types (SavedFields)
import Shared.Profile.Types as SPT
import Shared.Resource as SRS
import Shared.User (Gender)

tooYoungMessage ∷ String
tooYoungMessage = "You must be over 18 years old in order to use MeroChat"

fieldTooBigMessage ∷ String
fieldTooBigMessage = "Field exceeded max value"

profile ∷ Int → ServerEffect Payload
profile loggedUserId = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserId
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      pure
            { user: profileUser
            , countries
            , languages
            }

generateField ∷ SPT.What → ServerEffect String
generateField field = do
      case field of
            SPT.Name → ST.generateName
            SPT.Headline → ST.generateHeadline
            SPT.Description → ST.generateDescription

data SaveAvatar = Ignore | Save (Maybe String)

save ∷ Int → SavedFields → _
save loggedUserId fields = do
      avatar ← case fields.avatar of
            Nothing → pure $ Save Nothing
            Just base64 | DM.isJust (SF.fromBase64File base64) → Save <<< Just <$> SF.saveBase64File base64
            _ → pure Ignore
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap fields.age > eighteen) $ SR.throwBadRequest tooYoungMessage
      moreTags ← SDP.hasPrivilege loggedUserId MoreTags
      --keep the old logic to save fields individually in case we need to do it again
      SD.withTransaction $ \connection → do
            SPD.saveRequiredField connection loggedUserId Name (DS.take nameMaxCharacters fields.name.value) fields.name.generated
            SPD.saveRequiredField connection loggedUserId Headline (DS.take headlineMaxCharacters fields.headline.value) fields.headline.generated
            SPD.saveRequiredField connection loggedUserId Description (DS.take descriptionMaxCharacters fields.description.value) fields.description.generated
            case avatar of
                  Save a → SPD.saveField connection loggedUserId Avatar a
                  _ → pure unit
            SPD.saveField connection loggedUserId Birthday fields.age
            SPD.saveField connection loggedUserId Gender fields.gender
            SPD.saveField connection loggedUserId Country fields.country
            SPD.saveLanguages connection loggedUserId $ DA.take maxLanguages fields.languages
            let
                  numberTags
                        | moreTags = maxFinalTags
                        | otherwise = maxStartingTags
            SPD.saveTags connection loggedUserId <<< DA.take numberTags $ map (DS.take tagMaxCharacters) fields.tags
      pure $ case avatar of
            Save a → { avatar : a }
            Ignore -> { avatar : fields.avatar }
            _ → { avatar : Nothing }

