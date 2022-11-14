module Server.Profile.Action where

import Prelude
import Shared.Options.Profile

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Run as R
import Server.Database.Privileges as SDP
import Server.File as SF
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Types (Payload)
import Server.Response as SR
import Server.ThreeK as ST
import Server.Effect (ServerEffect)
import Shared.DateTime (DateWrapper)
import Shared.DateTime as SDT
import Shared.Privilege (Privilege(..))
import Shared.Profile.Types (What(..))
import Server.Sanitize as SS
import Shared.Resource (uploadedImagePath)
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

saveGeneratedField ∷ Int → What → Maybe String → ServerEffect String
saveGeneratedField loggedUserId field value = do
      finalValue ← case field of
            Name → DS.take nameMaxCharacters <$> DM.maybe ST.generateName pure value
            Headline → DS.take headlineMaxCharacters <$> DM.maybe ST.generateHeadline pure value
            Description → SS.sanitize <<< DS.take descriptionMaxCharacters <$> DM.maybe ST.generateDescription pure value
      SPD.saveField loggedUserId (show field) finalValue
      pure finalValue

saveAvatar ∷ Int → Maybe String → ServerEffect Unit
saveAvatar loggedUserId base64 = do
      avatar ← case base64 of
            Nothing → pure Nothing
            Just path → do
                  let fileName = DS.replace (Pattern uploadedImagePath) (Replacement "") path
                  --likely a base64 image
                  if fileName == path then
                        Just <$> SF.saveBase64File path
                  else
                        pure $ Just fileName
      SPD.saveField loggedUserId "avatar" avatar

saveAge ∷ Int → Maybe DateWrapper → ServerEffect Unit
saveAge loggedUserId birthday = do
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap birthday > eighteen) $ SR.throwBadRequest tooYoungMessage
      SPD.saveField loggedUserId "birthday" birthday

saveGender ∷ Int → Maybe Gender → ServerEffect Unit
saveGender loggedUserId gender = SPD.saveField loggedUserId "gender" gender

saveCountry ∷ Int → Maybe Int → ServerEffect Unit
saveCountry loggedUserId country = SPD.saveField loggedUserId "country" country

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