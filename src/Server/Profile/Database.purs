module Server.Profile.Database where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row0(..), Row1(..), Row2(..))
import Server.Database as SD
import Server.Types
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

presentProfile :: PrimaryKey -> ServerEffect ProfileUserWrapper
presentProfile loggedUserID = SD.single' (Query $ "select" <> profilePresentationFields <> "from" <> usersTable <> " where u.id = $1") $ Row1 loggedUserID

presentCountries :: ServerEffect (Array (Tuple PrimaryKey String))
presentCountries = SD.select (Query "select id, name from countries order by name") Row0

presentLanguages :: ServerEffect (Array (Tuple PrimaryKey String))
presentLanguages = SD.select (Query "select id, name from languages order by name") Row0

saveProfile :: { user :: ProfileUser, avatar :: Maybe String, languages :: Array PrimaryKey, tags :: Array String } -> ServerEffect Unit
saveProfile {
    user: { id, name, headline, description, country, gender, age },
    avatar,
    languages,
    tags
} = SD.withTransaction $ \connection -> void do
        SD.executeWith connection (Query """update users
                             set avatar = $2,
                                 name = $3,
                                 headline = $4,
                                 description = $5,
                                 country = $6,
                                 gender = $7,
                                 birthday = $8
                             where id = $1""") (id /\ avatar /\ name /\ headline /\ description /\ country /\ gender /\ (map (\(DateWrapper d) -> d) age))
        SD.executeWith connection (Query """delete from languages_users where speaker = $1""") $ Row1 id
        void $ DT.traverse (SD.executeWith connection (Query """insert into languages_users (speaker, language) values ($1, $2)""") <<< Row2 id) languages
        SD.executeWith connection (Query """delete from tags_users where creator = $1""") $ Row1 id
        tagIDs :: Array PrimaryKey <- DT.traverse (SD.scalarWith connection (Query """
            with ins as (insert into tags (name) values ($1) on conflict on constraint unique_tag do nothing returning id)
            select coalesce ((select id from ins), (select id from tags where name = $1))""") <<< Row1) tags
        DT.traverse (SD.executeWith connection (Query """insert into tags_users (creator, tag) values ($1, $2)""") <<< Row2 id) tagIDs

