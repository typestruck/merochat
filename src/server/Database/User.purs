module Server.Database.User where

import Server.Types
import Shared.Types
import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..))
import Server.Database as SD
import Debug.Trace(spy)

baseQuery :: String
baseQuery = "select id, email, password from users where "

userBy :: By -> ServerEffect (Maybe RegisterLoginUser)
userBy by =
        case by of
                Email value -> SD.single (Query $ baseQuery <> "email = $1") $ Row1 value
                ID value -> SD.single (Query $ baseQuery <> "id = $1") $ Row1 value