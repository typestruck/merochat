module Server.Profile.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.KarmaLeaderboard
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Profile.Database.Flat
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Functions
import Shared.Profile.Types (ProfileUser)
import Shared.Unsafe as SU

presentProfile ∷ Int → ServerEffect FlatProfileUser
presentProfile loggedUserID = map SU.fromJust <<< SD.single $ select profilePresentationFields # from source # wher filtered
      where
      profilePresentationFields = (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ _birthday
            /\ _name
            /\ _headline
            /\ _description
            /\ _country
            /\ (select (int_array_agg (l ... _id) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)))
            /\ (select (string_agg _name ("\n" # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)
      source = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)
      filtered = (u ... _id .=. loggedUserID)

presentCountries ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentCountries = SD.query $ select (_id /\ _name) # from countries # orderBy _name

presentLanguages ∷ ServerEffect (Array { id ∷ Int, name ∷ String })
presentLanguages = SD.query $ select (_id /\ _name) # from languages # orderBy _name

--refactor: add to droplet: with, on conflict
saveProfile ∷ { user ∷ ProfileUser, avatar ∷ Maybe String, languages ∷ Array Int, tags ∷ Array String } → ServerEffect Unit
saveProfile
      { user: { id, name, headline, description, country, gender, age }
      , avatar
      , languages
      , tags
      } = SD.withTransaction $ \connection → void do
      SD.executeWith connection $
            update users
                  # set
                        ( (_avatar .=. avatar)
                                /\ (_name .=. name)
                                /\ (_headline .=. headline)
                                /\ (_description .=. description)
                                /\ (_country .=. country)
                                /\ (_gender .=. gender)
                                /\
                                      (_birthday .=. map DN.unwrap age)
                        )
                  # wher (_id .=. id)
      SD.executeWith connection $ delete # from languages_users # wher (_speaker .=. id)
      void $ DT.traverse (\lang → SD.executeWith connection $ insert # into languages_users (_speaker /\ _language) # values (id /\ lang)) languages
      SD.executeWith connection $ delete # from tags_users # wher (_creator .=. id)
      tagIDs ∷ Array (Maybe { id ∷ Int }) ← DT.traverse
            ( \name → SD.unsafeSingleWith connection
                    ( """with ins as (insert into tags (name) values (@name) on conflict on constraint unique_tag do nothing returning id)
            select coalesce ((select id from ins), (select id from tags where name = @name)) as id"""
                    )
                    { name }
            )
            tags
      DT.traverse (\tid → SD.executeWith connection (insert # into tags_users (_creator /\ _tag) # values (id /\ (SU.fromJust tid).id))) tagIDs
