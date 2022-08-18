module Server.Profile.Action where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Run as R
import Server.File as SF
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Types (Payload)
import Server.Response as SR
import Server.ThreeK as ST
import Server.Types (ServerEffect)
import Shared.DateTime (DateWrapper)
import Shared.DateTime as SDT
import Shared.Options.File (imageBasePath)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, maxLanguages, maxTags, nameMaxCharacters, tagMaxCharacters)
import Shared.Profile.Types (ProfileUser, What(..))
import Shared.User (Gender)

tooManyTagsMessage ∷ String
tooManyTagsMessage = "Number of tags larger than " <> show maxTags <> " limit"

tooYoungMessage ∷ String
tooYoungMessage = "You must be over 13 years old in order to use MelanChat"

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
            Description → DS.take descriptionMaxCharacters <$> DM.maybe ST.generateDescription pure value
      SPD.saveField loggedUserId (show field) finalValue
      pure finalValue

saveAvatar ∷ Int → Maybe String → ServerEffect Unit
saveAvatar loggedUserId base64 = do
      avatar ← case base64 of
            Nothing → pure Nothing
            Just path → do
                  let fileName = DS.replace (Pattern $ imageBasePath <> "upload/") (Replacement "") path
                  --likely a base64 image
                  if fileName == path then
                        Just <$> SF.saveBase64File path
                  else
                        pure $ Just fileName
      SPD.saveField loggedUserId "avatar" avatar

saveAge ∷ Int → Maybe DateWrapper → ServerEffect Unit
saveAge loggedUserId birthday = do
      thirteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap birthday > thirteen) $ SR.throwBadRequest tooYoungMessage
      SPD.saveField loggedUserId "birthday" birthday

saveGender ∷ Int → Maybe Gender → ServerEffect Unit
saveGender loggedUserId gender = SPD.saveField loggedUserId "gender" gender

saveCountry ∷ Int → Maybe Int → ServerEffect Unit
saveCountry loggedUserId country = SPD.saveField loggedUserId "country" country

saveLanguages ∷ Int → Maybe (Array Int) → ServerEffect Unit
saveLanguages loggedUserId languages = SPD.saveLanguages loggedUserId <<< DA.take maxLanguages $ DM.fromMaybe [] languages

saveProfile ∷ Int → ProfileUser → ServerEffect Unit
saveProfile loggedUserId profileUser@{ name, age, headline, description, avatar, languages, tags } = do
      when anyFieldIsTooBig $ SR.throwBadRequest fieldTooBigMessage
      when (DA.length tags > maxTags) $ SR.throwBadRequest tooManyTagsMessage
      thirteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap age > thirteen) $ SR.throwBadRequest tooYoungMessage

      updatedAvatar ← case avatar of
            Nothing → pure Nothing
            Just path →
                  let
                        fileName = DS.replace (Pattern $ imageBasePath <> "upload/") (Replacement "") path
                  in
                        --likely a base64 image
                        if fileName == path then
                              Just <$> SF.saveBase64File path
                        else
                              pure $ Just fileName
      SPD.saveProfile
            { user: profileUser { id = loggedUserId }
            , avatar: updatedAvatar
            , languages
            , tags
            }
      where
      anyFieldIsTooBig = DS.length name > nameMaxCharacters || DS.length headline > headlineMaxCharacters || DS.length description > descriptionMaxCharacters || DA.any ((_ > tagMaxCharacters) <<< DS.length) tags