module Server.Profile.Database where

import Prelude

import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLValue, Pool, Query(..), Row0(..), Row1(..))
import Server.Database as SD
import Server.Types (ServerEffect)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (MDate(..), PrimaryKey)

profilePresentationFields :: String
profilePresentationFields = """ u.id,
avatar,
gender,
birthday,
name,
headline,
description,
country,
(select string_agg(cast(l.id as varchar), ','  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags """

presentProfile :: PrimaryKey -> ServerEffect ProfileUser
presentProfile id = SD.single' (Query $ "select" <> profilePresentationFields <> "from users u where id = $1") $ Row1 id

presentCountries :: ServerEffect (Array (Tuple Int String))
presentCountries = SD.select (Query "select id, name from countries order by name") Row0

presentLanguages :: ServerEffect (Array (Tuple Int String))
presentLanguages = SD.select (Query "select id, name from languages order by name") Row0

saveProfile :: { user :: ProfileUser, languages :: Array Int } -> ServerEffect Unit
saveProfile {
    user: ProfileUser { id, avatar, name, headline, description, country, gender, birthday },
    languages
} = SD.withTransaction $ \connection -> void do
        SD.executeWith connection (Query """update users
                             set avatar = $2,
                                 name = $3,
                                 headline = $4,
                                 description = $5,
                                 country = $6,
                                 gender = $7,
                                 birthday = $8
                             where id = $1""") (id /\ avatar /\ name /\ headline /\ description /\ country /\ gender /\ (map (\(MDate d) -> d) birthday))
        SD.executeWith connection (Query """delete from languagesUsers where speaker = $1""") $ Row1 id
        DT.traverse (\lang -> SD.executeWith connection (Query """insert into languagesUsers (speaker, language) values ($1, $2) on conflict on constraint uniqueUserLanguage do nothing""") (id /\ lang)) languages

