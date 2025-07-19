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
import Debug (spy)
import Run as R
import Server.Database.CompleteProfiles as CP
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
import Shared.Profile.Types (What(..), SavedFields)
import Shared.Resource (Media(..), ResourceType(..))
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

generateField ∷ What → ServerEffect String
generateField field = do
      case field of
            Name → ST.generateName
            Headline → ST.generateHeadline
            Description → ST.generateDescription

save :: Int → SavedFields → ServerEffect Unit
save loggedUserId fields = do
      avatar ← case fields.avatar of
            Nothing → pure Nothing
            Just base64 → Just <$> SF.saveBase64File base64
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap fields.birthday > eighteen) $ SR.throwBadRequest tooYoungMessage
      SPD.saveLanguages loggedUserId <<< DA.take maxLanguages $ DM.fromMaybe [] fields.languages
      moreTags ← SDP.hasPrivilege loggedUserId MoreTags
      let
            numberTags
                  | moreTags = maxFinalTags
                  | otherwise = maxStartingTags
      SPD.saveTags loggedUserId <<< DA.take numberTags <<< map (DS.take tagMaxCharacters) $ DM.fromMaybe [] tags
      pure ok

