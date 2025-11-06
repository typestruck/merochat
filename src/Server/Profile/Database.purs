module Server.Profile.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Badges
import Server.Database.BadgesUsers
import Server.Database.CompleteProfiles
import Server.Database.Privileges

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver.Internal.Query (Connection(..))
import Prelude as P
import Server.Database as SD
import Server.Database.Countries (countries)
import Server.Database.Fields (_date, _id, _name, b, bu, k, p, l, lu, onlineStatus, p, tu, u)
import Server.Database.KarmaLeaderboard (_current_karma, _karma, _karmaPosition, _position, _ranker, karma_leaderboard)
import Server.Database.Languages (_languages, languages)
import Server.Database.LanguagesUsers (_language, _speaker, languages_users)
import Server.Database.Messages (_content)
import Server.Database.ModeratedProfileFields (_avatared, _descriptioned, _headlined, _moderated, _named, moderated_profile_fields)
import Server.Database.Posts (_expires, _poster, posts)
import Server.Database.Tags (_tags, tags)
import Server.Database.TagsUsers (_creator, _tag, tags_users)
import Server.Database.Users (_avatar, _birthday, _country, _description, _gender, _headline, _onlineStatus, users)
import Server.Effect (ServerEffect)
import Server.Profile.Database.Flat (FlatProfileUser)
import Shared.Post (Post)
import Shared.ProfileColumn as CP
import Shared.Unsafe as SU
import Simple.JSON as SJ
import Type.Proxy (Proxy(..))

presentProfile ∷ Int → ServerEffect FlatProfileUser
presentProfile loggedUserId = map SU.fromJust <<< SD.single $ select profilePresentationFields # from source # wher filtered
      where
      profilePresentationFields = (u ... _id # as _id)
            /\ _avatared
            /\  _gender
            /\ _birthday
            /\ _named
            /\ _headlined
            /\ (_onlineStatus # as onlineStatus)
            /\ _descriptioned
            /\ _country
            /\ (select (array_agg _feature # as _privileges) # from privileges # wher (_quantity .<=. k ... _current_karma) # orderBy _privileges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _id) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _name # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
            /\ (select (array_agg _description # as _badges) # from (((badges # as b) `join` (badges_users # as bu)) # on (b ... _id .=. bu ... _badge .&&. bu ... _receiver .=. u ... _id)) # orderBy _badges # limit (Proxy ∷ _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)
      source = join (join (users # as u) (moderated_profile_fields # as p) # on (u ... _id .=. p ... _moderated)) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)
      filtered = (u ... _id .=. loggedUserId)

presentCountries ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentCountries = SD.query $ select (_id /\ _name) # from countries # orderBy _name

presentPosts ∷ Int → Maybe Int → ServerEffect (Array Post)
presentPosts loggedUserId after =
      case after of
            Just id → SD.query $ select (_id /\ _date /\ _expires /\ _content) # from posts # wher (_poster .=. loggedUserId .&&. _id .>. id) # orderBy (_date # desc) # limit (Proxy ∷ _ 8)
            Nothing → SD.query $ select (_id /\ _date /\ _expires /\ _content) # from posts # wher (_poster .=. loggedUserId) # orderBy (_date # desc) # limit (Proxy ∷ _ 8)

presentLanguages ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentLanguages = SD.query $ select (_id /\ _name) # from languages # orderBy _name

presentGeneratedFields ∷ Connection → Int → _
presentGeneratedFields connection loggedUserId = map SU.fromJust <<< SD.singleWith connection $ select (_name /\ _headline /\ _description) # from users # wher (_id .=. loggedUserId)

upsertCompletness ∷ Connection → Int → CP.ProfileColumn → _
upsertCompletness connection loggedUserId field = SD.unsafeExecuteWith connection "INSERT INTO complete_profiles(completer, completed) values(@loggedUserId, @field) ON CONFLICT (completer, completed) DO NOTHING" { loggedUserId, field }

removeCompletness ∷ Connection → Int → CP.ProfileColumn → _
removeCompletness connection loggedUserId field = SD.executeWith connection $ delete # from complete_profiles # wher (_completer .=. loggedUserId .&&. _completed .=. field)

saveRequiredField ∷ ∀ t. ToValue t ⇒ Connection → Int → CP.ProfileColumn → t → Boolean → _
saveRequiredField connection loggedUserId field value generated = do
      SD.unsafeExecuteWith connection ("UPDATE users SET " <> show field <> " = @value WHERE id = @loggedUserId") { value, loggedUserId }
      if generated then
            removeCompletness connection loggedUserId field
      else
            upsertCompletness connection loggedUserId field

fieldForApproval = case _ of
      CP.Name → "named"
      CP.Description → "descriptioned"
      CP.Avatar → "avatared"
      CP.Headline → "headlined"
      CP.ChatBackground -> "chat_backgrounded"
      _ → ""

saveForApproval connection loggedUserId field value = SD.unsafeExecuteWith connection ("UPDATE moderated_profile_fields SET " <> fieldForApproval field <> " = @value WHERE moderated = @loggedUserId") { value, loggedUserId }

saveField ∷ ∀ t. ToValue t ⇒ Connection → Int → CP.ProfileColumn → Maybe t → _
saveField connection loggedUserId field value = do
      SD.unsafeExecuteWith connection ("UPDATE users SET " <> show field <> " = @value WHERE id = @loggedUserId") { value, loggedUserId }
      case value of
            Just _ → upsertCompletness connection loggedUserId field
            Nothing → removeCompletness connection loggedUserId field

saveLanguages connection loggedUserId languages = void do
      SD.executeWith connection $ delete # from languages_users # wher (_speaker .=. loggedUserId)
      if DA.null languages then
            removeCompletness connection loggedUserId CP.Languages
      else do
            upsertCompletness connection loggedUserId CP.Languages
            SD.executeWith connection $ insert # into languages_users (_speaker /\ _language) # values (map (loggedUserId /\ _) languages)

saveTags connection loggedUserId tags = void do
      SD.executeWith connection $ delete # from tags_users # wher (_creator .=. loggedUserId)
      if DA.null tags then
            removeCompletness connection loggedUserId CP.Tags
      else do
            upsertCompletness connection loggedUserId CP.Tags
            -- update anyway so we can have a returning for all rows
            tagIds ∷ Array { id ∷ Int } ← SD.unsafeQueryWith connection "INSERT INTO tags (name) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (tag text)) ON CONFLICT ON CONSTRAINT unique_tag DO UPDATE SET name = excluded.name RETURNING id" { jsonInput: SJ.writeJSON $ map { tag: _ } tags }
            SD.executeWith connection $ insert # into tags_users (_creator /\ _tag) # values (map ((loggedUserId /\ _) <<< _.id) tagIds)
