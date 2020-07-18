module Server.Recover.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..))
import Prelude
import Server.Database as SD

insertRecover :: { email :: String, token :: String } -> ServerEffect Unit
insertRecover { email, token } = void $ SD.insert (Query "insert into recoveries (uuid, recoverer) values($1, $2)") (token /\ email)