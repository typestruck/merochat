module Server.Profile.Database where

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLValue, Pool, Query(..), Row1(..), Row2(..), Row3(..))
import Prelude (Unit, bind, pure, ($), (<<<), (<>))
import Server.Database as SD
import Server.IM.Database (presentUserQuery)
import Server.IM.Database as SIM
import Server.Types (ServerEffect, BaseEffect)
import Shared.IM.Types (HistoryMessage, IMUser)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (PrimaryKey)

presentUser :: PrimaryKey -> ServerEffect ProfileUser
presentUser id = SD.single' presentUserQuery $ SIM.presentUserParameters id
