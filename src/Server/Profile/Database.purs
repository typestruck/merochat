module Server.Profile.Database where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Types
import Server.Database.Fields
import Server.Database.User
import Server.Database.LanguagesUsers
import Droplet.Language
import Server.Database.TagsUsers
import Shared.Types

profilePresentationFields :: String
profilePresentationFields = """ u.id,
avatar,
gender,
birthday,
name,
headline,
description,
country,
(select array_agg(l.id) from languages l join languages_users lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by l.id) from tags l join tags_users tu on l.id = tu.tag and tu.creator = u.id ) tags,
k.current_karma karma,
k.position """

usersTable :: String
usersTable = " users u join karma_leaderboard k on u.id = k.ranker "

presentProfile :: Int -> ServerEffect ProfileUserWrapper
presentProfile loggedUserID = SD.single' ("select" <> profilePresentationFields <> "from" <> usersTable <> " where u.id = $1") $ Row1 loggedUserID

presentCountries :: ServerEffect (Array (Tuple Int String))
presentCountries = SD.unsafeQuery ("select id, name from countries order by name") {}

presentLanguages :: ServerEffect (Array (Tuple Int String))
presentLanguages = SD.unsafeQuery ("select id, name from languages order by name") {}

saveProfile :: { user :: ProfileUser, avatar :: Maybe String, languages :: Array Int, tags :: Array String } -> ServerEffect Unit
saveProfile {
      user: { id, name, headline, description, country, gender, age },
      avatar,
      languages,
      tags
} = SD.withTransaction $ \connection -> void do
      SD.executeWith connection $
            update users # set
                  ((_avatar /\ avatar) /\
                   (_name /\ name) /\
                   (_headline /\ headline) /\
                   (_description /\ description) /\
                   (_country /\ country) /\
                   (_gender /\ gender) /\
                   (_birthday /\ age))
                  # wher (_id .=. id)
      SD.executeWith connection $ delete # from languages_users # wher (_speaker .=. id)
      void $ DT.traverse (SD.executeWith connection $ insert # into languages_users (_speaker /\ _language) # values (id /\ languages))
      SD.executeWith connection $ delete # from tags_users # wher (_creator .=. id)
      tagIDs :: Array Int <- DT.traverse (SD.scalarWith connection ("""
            with ins as (insert into tags (name) values (@name) on conflict on constraint unique_tag do nothing returning id)
            select coalesce ((select id from ins), (select id from tags where name = @name))""") <<< { name: _ }) tags
      DT.traverse (SD.executeWith connection $ insert # into # tags_users (_creator /\ _tag) # values (id /\ _)) tagIDs

