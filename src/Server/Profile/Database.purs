module Server.Profile.Database where

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLValue, Pool, Query(..), Row0(..), Row1(..))
import Prelude (Unit, bind, pure, ($), (<<<), (<>))
import Server.Database as SD
import Server.Types (ServerEffect)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (PrimaryKey)

profilePresentationFields :: String
profilePresentationFields = """ u.id,
avatar,
gender,
birthday,
name,
headline,
description,
country,
(select string_agg(l.name, ','  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags """

presentProfile :: PrimaryKey -> ServerEffect ProfileUser
presentProfile id = SD.single' (Query $ "select" <> profilePresentationFields <> "from users u where id = $1") $ Row1 id

presentCountries :: ServerEffect (Array (Tuple Int String))
presentCountries = SD.select (Query "select id, name from countries order by name") Row0

saveProfile :: ProfileUser -> ServerEffect Unit
saveProfile (ProfileUser { id, avatar, name, headline, description, country, gender }) =
        SD.execute (Query """update users
                             set avatar = $2,
                                 name = $3,
                                 headline = $4,
                                 description = $5,
                                 country = $6,
                                 gender = $7
                             where id = $1""") (id /\ avatar /\ name /\ headline /\ description /\ country /\ gender)
