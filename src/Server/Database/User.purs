module Server.Database.User (userBy) where

import Server.Types
import Shared.Types
import Prelude
import Data.Maybe (Maybe)
import Database.PostgreSQL (Query(..), Row1(..))
import Server.Database as SD

baseQuery :: String
baseQuery = "select id, email, password from users where active and "

userBy :: By -> ServerEffect (Maybe RegisterLoginUser)
userBy = case _ of
      Email value -> SD.single (Query $ baseQuery <> "lower(email) = lower($1)") $ Row1 value
      ID value -> SD.single (Query $ baseQuery <> "id = $1") $ Row1 value