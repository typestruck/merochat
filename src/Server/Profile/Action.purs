module Server.Profile.Action where

import Prelude
import Shared.Options.Profile

import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DST
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Run as R
import Run.Except as RE
import Safe.Coerce as SC
import Server.Asks.Database as SAD
import Server.Database as SD
import Server.Database.Privileges as SDP
import Server.Effect (ServerEffect)
import Server.Email (Email(..))
import Server.Email as SE
import Server.File as SF
import Server.Profile.BadWords (badWords)
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Response as SR
import Server.Generate as ST
import Shared.Ask (Ask)
import Shared.DateTime as SDT
import Shared.File as SSF
import Shared.Options.Ask (maxAskCharacters)
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Profile.Types (SavedFields)
import Shared.Profile.Types as SPT
import Shared.ProfileColumn (ProfileColumn(..))
import Shared.Resource (localBasePath, uploadFolder)
import Shared.ResponseError (ResponseError(..))

tooYoungMessage ∷ String
tooYoungMessage = "You must be over 18 years old in order to use MeroChat"

fieldTooBigMessage ∷ String
fieldTooBigMessage = "Field exceeded max value"

profile ∷ Int → ServerEffect _
profile loggedUserId = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserId
      posts ← SPD.presentPosts loggedUserId Nothing
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      asks <- SAD.presentAllAsks loggedUserId Nothing
      pure
            { user: profileUser
            , countries
            , posts
            , asks
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

refreshAsks ∷ Int → Maybe Int → ServerEffect (Array Ask)
refreshAsks loggedUserId id = SAD.presentAllAsks loggedUserId id

saveAnswer :: Int -> Int -> String -> ServerEffect Unit
saveAnswer loggedUserId askId answer = do
      let trimmed = DS.trim answer
      when (DS.null trimmed || DS.length trimmed > maxAskCharacters * 2) <<< RE.throw $ BadRequest { reason : "invalid answer"}
      SPD.saveAnswer loggedUserId askId answer

data SaveAvatar = Ignore | Save (Maybe String)

save ∷ Int → SavedFields → _
save loggedUserId fields = do
      avatar ← case fields.avatar of
            Nothing → pure $ Save Nothing
            Just base64 | DM.isJust (SSF.fromBase64File base64) → Save <<< Just <$> SF.saveBase64File base64
            _ → pure Ignore
      eighteen ← Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map SC.coerce fields.age > eighteen) $ SR.throwBadRequest tooYoungMessage
      moreTags ← SDP.hasPrivilege loggedUserId MoreTags
      badWordedFields ← SD.withTransaction $ \connection → do
            current ← SPD.presentGeneratedFields connection loggedUserId
            let name = DS.take nameMaxCharacters $ DS.trim fields.name.value
            nameHasBadWord ←
                  if name == current.name then
                        pure Nothing
                  else if hasBadWords name then do
                        SPD.saveForApproval connection loggedUserId Name name
                        pure <<< Just $ Name /\ name
                  else do
                        SPD.saveRequiredField connection loggedUserId Name name fields.name.generated
                        pure Nothing
            let headline = DS.take headlineMaxCharacters $ DS.trim fields.headline.value
            headlineHasBadWord ←
                  if headline == current.headline then
                        pure Nothing
                  else if hasBadWords headline then do
                        SPD.saveForApproval connection loggedUserId Headline headline
                        pure <<< Just $ Headline /\ headline
                  else do
                        SPD.saveRequiredField connection loggedUserId Headline headline fields.headline.generated
                        pure Nothing
            let description = DS.take descriptionMaxCharacters $ DS.trim fields.description.value
            descriptionHasBadWord ←
                  if description == current.description then
                        pure Nothing
                  else if hasBadWords description then do
                        SPD.saveForApproval connection loggedUserId Description description
                        pure <<< Just $ Description /\ description
                  else do
                        SPD.saveRequiredField connection loggedUserId Description description fields.description.generated
                        pure Nothing
            avatarIsNaughty ← case avatar of
                  Save (Just fileName) → do
                        isNaughty ← R.liftAff <<< SF.isNsfw $ localBasePath <> uploadFolder <> fileName
                        if isNaughty then do
                              SPD.saveForApproval connection loggedUserId Avatar (spy ("naughty avatar for " <> show loggedUserId) fileName)
                              pure <<< Just $ Avatar /\ fileName
                        else do
                              SPD.saveField connection loggedUserId Avatar $ Just fileName
                              pure Nothing
                  Save Nothing → do
                        SPD.saveField connection loggedUserId Avatar (Nothing ∷ Maybe String)
                        pure Nothing
                  _ → pure Nothing
            SPD.saveField connection loggedUserId Birthday fields.age
            SPD.saveField connection loggedUserId Gender fields.gender
            SPD.saveField connection loggedUserId Country fields.country
            SPD.saveLanguages connection loggedUserId $ DA.take maxLanguages fields.languages
            let
                  numberTags
                        | moreTags = maxFinalTags
                        | otherwise = maxStartingTags
            SPD.saveTags connection loggedUserId <<< DA.take numberTags $ map (DS.take tagMaxCharacters) fields.tags
            pure [ nameHasBadWord, headlineHasBadWord, descriptionHasBadWord, avatarIsNaughty ]
      --if a user inputs some bad word into their profile we save it on a different table that is shown only to that user and then needs to be approved
      DF.traverse_ (\b → SE.sendEmail (Approve { user_id: loggedUserId, field: show $ DT.fst b, value: DT.snd b })) $ DA.catMaybes badWordedFields
      pure $ case avatar of
            Save a → { avatar: a }
            Ignore → { avatar: fields.avatar }

hasBadWords ∷ String → Boolean
hasBadWords phrase = DA.any (flip DST.member set) badWords
      where
      set = DST.fromFoldable $ DS.split (Pattern " ") $ DS.toLower phrase

ignoreAsk :: Int -> Int -> ServerEffect Unit
ignoreAsk loggedUserId id = SPD.ignoreAsk loggedUserId id
