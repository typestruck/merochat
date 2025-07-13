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
import Shared.Profile.Types (What(..))
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

saveAvatar ∷ Int → Maybe String → ServerEffect Unit
saveAvatar loggedUserId base64 = do
      avatar ← case base64 of
            Nothing → pure Nothing
            Just path → Just <$> SF.saveBase64File path
      SPD.saveField loggedUserId CP.Avatar avatar

saveAge ∷ Int → Maybe DateWrapper → ServerEffect Unit
saveAge loggedUserId birthday = do
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap birthday > eighteen) $ SR.throwBadRequest tooYoungMessage
      SPD.saveField loggedUserId CP.Birthday birthday

saveGender ∷ Int → Maybe Gender → ServerEffect Unit
saveGender loggedUserId gender = SPD.saveField loggedUserId CP.Gender gender

saveCountry ∷ Int → Maybe Int → ServerEffect Unit
saveCountry loggedUserId country = SPD.saveField loggedUserId CP.Country country

saveLanguages ∷ Int → Maybe (Array Int) → ServerEffect Unit
saveLanguages loggedUserId languages = SPD.saveLanguages loggedUserId <<< DA.take maxLanguages $ DM.fromMaybe [] languages

saveTags ∷ Int → Maybe (Array String) → ServerEffect Unit
saveTags loggedUserId tags = do
      moreTags ← SDP.hasPrivilege loggedUserId MoreTags
      let
            numberTags
                  | moreTags = maxFinalTags
                  | otherwise = maxStartingTags
      SPD.saveTags loggedUserId <<< DA.take numberTags <<< map (DS.take tagMaxCharacters) $ DM.fromMaybe [] tags

