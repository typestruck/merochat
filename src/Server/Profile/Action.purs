module Server.Profile.Action where

import Prelude
import Shared.Options.Profile

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String as DS
import Debug (spy)
import Run as R
import Server.Database as SD
import Server.Database.Privileges as SDP
import Server.Effect (ServerEffect)
import Server.File as SF
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Types (Payload)
import Server.Response as SR
import Server.ThreeK as ST
import Shared.DateTime as SDT
import Shared.File as SSF
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Profile.Types (SavedFields)
import Shared.Profile.Types as SPT
import Shared.ProfileColumn (ProfileColumn(..))

tooYoungMessage ∷ String
tooYoungMessage = "You must be over 18 years old in order to use MeroChat"

fieldTooBigMessage ∷ String
fieldTooBigMessage = "Field exceeded max value"

profile ∷ Int → ServerEffect Payload
profile loggedUserId = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserId
      posts ← SPD.presentPosts loggedUserId Nothing
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      pure
            { user: profileUser
            , countries
            , posts
            , languages
            }

generateField ∷ SPT.What → ServerEffect String
generateField field = do
      case field of
            SPT.Name → ST.generateName
            SPT.Headline → ST.generateHeadline
            SPT.Description → ST.generateDescription

refreshPosts ∷ Int → Maybe Int → ServerEffect (Array Post)
refreshPosts loggedUserId id = SPD.presentPosts loggedUserId id

data SaveAvatar = Ignore | Save (Maybe String)

save ∷ Int → SavedFields → _
save loggedUserId fields = do
      avatar ← case fields.avatar of
            Nothing → pure $ Save Nothing
            Just base64 | DM.isJust (SSF.fromBase64File base64) → Save <<< Just <$> SF.saveBase64File base64
            _ → pure Ignore
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap fields.age > eighteen) $ SR.throwBadRequest tooYoungMessage
      moreTags ← SDP.hasPrivilege loggedUserId MoreTags
      --keep the old logic to save fields individually in case we need to do it again
      SD.withTransaction $ \connection → do
            current ← SPD.presentGeneratedFields connection loggedUserId
            let name = DS.take nameMaxCharacters $ DS.trim fields.name.value
            unless (name == current.name) $ SPD.saveRequiredField connection loggedUserId Name name fields.name.generated
            let headline = DS.take headlineMaxCharacters $ DS.trim fields.headline.value
            unless (headline == current.headline) $ SPD.saveRequiredField connection loggedUserId Headline headline fields.headline.generated
            let description = DS.take descriptionMaxCharacters $ DS.trim fields.description.value
            unless (description == current.description) $ SPD.saveRequiredField connection loggedUserId Description description fields.description.generated
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
            Save a → { avatar: a }
            Ignore → { avatar: fields.avatar }

