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
(select array_agg(l.id) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags,
(select sum(amount) from karmaHistories where target = u.id) karma """

presentProfile :: PrimaryKey -> ServerEffect ProfileUserWrapper
presentProfile id = SD.single' (Query $ "select" <> profilePresentationFields <> "from users u where id = $1") $ Row1 id

presentCountries :: ServerEffect (Array (Tuple PrimaryKey String))
presentCountries = SD.select (Query "select id, name from countries order by name") Row0

presentLanguages :: ServerEffect (Array (Tuple PrimaryKey String))
presentLanguages = SD.select (Query "select id, name from languages order by name") Row0

saveProfile :: { user :: ProfileUser, avatar :: Maybe String, languages :: Array PrimaryKey, tags :: Array String } -> ServerEffect Unit
saveProfile {
    user: { id, name, headline, description, country, gender, birthday },
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
                             where id = $1""") (id /\ avatar /\ name /\ headline /\ description /\ country /\ gender /\ (map (\(DateWrapper d) -> d) birthday))
        SD.executeWith connection (Query """delete from languagesUsers where speaker = $1""") $ Row1 id
        void $ DT.traverse (SD.executeWith connection (Query """insert into languagesUsers (speaker, language) values ($1, $2)""") <<< Row2 id) languages
        SD.executeWith connection (Query """delete from tagsUsers where creator = $1""") $ Row1 id
        tagIDs :: Array PrimaryKey <- DT.traverse (SD.scalarWith connection (Query """
            with ins as (insert into tags (name) values ($1) on conflict on constraint uniqueTag do nothing returning id)
            select coalesce ((select id from ins), (select id from tags where name = $1))""") <<< Row1) tags
        DT.traverse (SD.executeWith connection (Query """insert into tagsUsers (creator, tag) values ($1, $2)""") <<< Row2 id) tagIDs

