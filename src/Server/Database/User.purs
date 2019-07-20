module Server.Database.User where

import Data.Int53
import Server.Types
import Shared.Types
import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..))
import Server.Database as SD

userBy :: By -> ServerEffect (Maybe User)
userBy by =
        case by of
                Email value -> SD.scalar (Query "select * from users where email = $1") $ Row1 value
                ID value -> SD.scalar (Query "select * from users where id = $1") $ Row1 value