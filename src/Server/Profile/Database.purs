module Server.Profile.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.Functions
import Server.Database.KarmaLeaderboard
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Profile.Database.Flat
import Server.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Reflectable (class Reflectable)
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Prelude as P
import Prim.Row (class Cons)
import Server.Database as SD
import Shared.Profile.Types (ProfileUser)
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
            /\ (select (int_array_agg (l ... _id) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ (select (string_agg _name ("\n" # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
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
