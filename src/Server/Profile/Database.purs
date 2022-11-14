module Server.Profile.Database where

import Server.Database.Privileges

import Data.Array as DA
import Data.Tuple.Nested ((/\))
import Droplet.Language
import Prelude (Unit, bind, discard, map, void, when, (#), ($), (<<<), (<>))
import Prelude as P
import Server.Database as SD
import Server.Database.Countries (countries)
import Server.Database.Fields (_id, _name, k, l, lu, tu, u)
import Server.Database.KarmaLeaderboard (_current_karma, _karma, _karmaPosition, _position, _ranker, karma_leaderboard)
import Server.Database.Languages (_languages, languages)
import Server.Database.LanguagesUsers (_language, _speaker, languages_users)
import Server.Database.Tags (_tags, tags)
import Server.Database.TagsUsers (_creator, _tag, tags_users)
import Server.Database.Users (_avatar, _birthday, _country, _description, _gender, _headline, users)
import Server.Profile.Database.Flat (FlatProfileUser)
import Server.Effect (ServerEffect)
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
            /\ _description
            /\ _country
            /\ (select (array_agg _feature # as _privileges) # from privileges # wher (_quantity .<=. k ... _current_karma) # orderBy _privileges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _id) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _name # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)
      source = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)
      filtered = (u ... _id .=. loggedUserId)

presentCountries ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentCountries = SD.query $ select (_id /\ _name) # from countries # orderBy _name

presentLanguages ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentLanguages = SD.query $ select (_id /\ _name) # from languages # orderBy _name

saveField ∷ ∀ t. ToValue t ⇒ Int → String → t → ServerEffect Unit
saveField loggedUserId field value = SD.unsafeExecute ("UPDATE users SET " <> field <> " = @value WHERE id = @loggedUserId") { value, loggedUserId }

saveLanguages ∷ Int → Array Int → ServerEffect Unit
saveLanguages loggedUserId languages = SD.withTransaction $ \connection → void do
      SD.executeWith connection $ delete # from languages_users # wher (_speaker .=. loggedUserId)
      when (P.not $ DA.null languages) $ SD.executeWith connection $ insert # into languages_users (_speaker /\ _language) # values (map (loggedUserId /\ _) languages)

saveTags ∷ Int → Array String → ServerEffect Unit
saveTags loggedUserId tags = SD.withTransaction $ \connection → void do
      SD.executeWith connection $ delete # from tags_users # wher (_creator .=. loggedUserId)
      when (P.not $ DA.null tags) do
            -- update anyway so we can have a returning for all rows
            tagIds ∷ Array { id ∷ Int } ← SD.unsafeQueryWith connection "INSERT INTO tags (name) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (tag text)) ON CONFLICT ON CONSTRAINT unique_tag DO UPDATE SET name = excluded.name RETURNING id" { jsonInput: SJ.writeJSON $ map { tag: _ } tags }
            SD.executeWith connection $ insert # into tags_users (_creator /\ _tag) # values (map ((loggedUserId /\ _) <<< _.id) tagIds)
