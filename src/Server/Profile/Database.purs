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
import Server.Database.CompleteProfiles as CP
import Server.Database.Countries (countries)
import Server.Database.Fields (_id, _name, b, bu, k, l, lu, onlineStatus, tu, u)
import Server.Database.KarmaLeaderboard (_current_karma, _karma, _karmaPosition, _position, _ranker, karma_leaderboard)
import Server.Database.Languages (_languages, languages)
import Server.Database.LanguagesUsers (_language, _speaker, languages_users)
import Server.Database.Tags (_tags, tags)
import Server.Database.TagsUsers (_creator, _tag, tags_users)
import Server.Database.Users (_avatar, _birthday, _country, _description, _gender, _headline, _onlineStatus, users)
import Server.Effect (ServerEffect)
import Server.Profile.Database.Flat (FlatProfileUser)
import Shared.Unsafe as SU
import Simple.JSON as SJ
import Type.Proxy (Proxy(..))

presentProfile ∷ Int → ServerEffect FlatProfileUser
presentProfile loggedUserId = map SU.fromJust <<< SD.single $ select profilePresentationFields # from source # wher filtered
      where
      profilePresentationFields = (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ _birthday
            /\ _name
            /\ _headline
            /\ (_onlineStatus # as onlineStatus)
            /\ _description
            /\ _country
            /\ (select (array_agg _feature # as _privileges) # from privileges # wher (_quantity .<=. k ... _current_karma) # orderBy _privileges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _id) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _name # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
            /\ (select (array_agg _description # as _badges) # from (((badges # as b) `join` (badges_users # as bu)) # on (b ... _id .=. bu ... _badge .&&. bu ... _receiver .=. u ... _id)) # orderBy _badges # limit (Proxy ∷ _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)
      source = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)
      filtered = (u ... _id .=. loggedUserId)

presentCountries ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentCountries = SD.query $ select (_id /\ _name) # from countries # orderBy _name

presentLanguages ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentLanguages = SD.query $ select (_id /\ _name) # from languages # orderBy _name

upsertCompletness ∷ Connection → Int → CP.ProfileColumn → _
upsertCompletness connection loggedUserId field = SD.unsafeExecuteWith connection "INSERT INTO complete_profiles(completer, completed) values(@loggedUserId, @field) ON CONFLICT (completer, completed) DO NOTHING" { loggedUserId, field }

removeCompletness ∷ Connection → Int → CP.ProfileColumn → _
removeCompletness connection loggedUserId field = SD.executeWith connection $ delete # from complete_profiles # wher (_completer .=. loggedUserId .&&. _completed .=. field)

saveRequiredField ∷ ∀ t. ToValue t ⇒ Connection -> Int → CP.ProfileColumn →  t -> Boolean  → _
saveRequiredField connection loggedUserId field value generated  =  do
      SD.unsafeExecuteWith connection ("UPDATE users SET " <> show field <> " = @value WHERE id = @loggedUserId") { value, loggedUserId }
      if generated then
            removeCompletness connection loggedUserId field
      else
            upsertCompletness connection loggedUserId field

saveField ∷ ∀ t. ToValue t ⇒ Connection -> Int → CP.ProfileColumn → Maybe t → _
saveField connection loggedUserId field value =  do
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

saveTags connection loggedUserId tags =  void do
      SD.executeWith connection $ delete # from tags_users # wher (_creator .=. loggedUserId)
      if DA.null tags then
            removeCompletness connection loggedUserId CP.Tags
      else do
            upsertCompletness connection loggedUserId CP.Tags
            -- update anyway so we can have a returning for all rows
            tagIds ∷ Array { id ∷ Int } ← SD.unsafeQueryWith connection "INSERT INTO tags (name) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (tag text)) ON CONFLICT ON CONSTRAINT unique_tag DO UPDATE SET name = excluded.name RETURNING id" { jsonInput: SJ.writeJSON $ map { tag: _ } tags }
            SD.executeWith connection $ insert # into tags_users (_creator /\ _tag) # values (map ((loggedUserId /\ _) <<< _.id) tagIds)
