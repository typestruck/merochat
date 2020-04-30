module Server.Profile.Database where

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLValue, Pool, Query(..))
import Prelude (Unit, bind, pure, ($), (<<<), (<>))
import Server.Database as SD
import Data.Tuple.Nested((/\))
import Server.IM.Database (presentUserQuery)
import Server.IM.Database as SIM
import Server.Types (ServerEffect, BaseEffect)
import Shared.IM.Types (HistoryMessage, IMUser)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (PrimaryKey)

presentProfile :: PrimaryKey -> ServerEffect ProfileUser
presentProfile id = SD.single' presentUserQuery $ SIM.presentUserParameters id

saveProfile :: ProfileUser -> ServerEffect Unit
saveProfile (ProfileUser { id, avatar, name, headline }) =
        SD.execute (Query """update users
                             set avatar = $2,
                                 name = $3,
                                 headline = $4
                             where id = $1""") (id /\ avatar /\ name /\ headline)
