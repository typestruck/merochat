module Server.Settings.Database where

import Prelude

import Data.Tuple.Nested ((/\))
import Database.PostgreSQL ( Query(..))
import Server.Database as SD
import Server.Types (ServerEffect)
import Shared.Types (PrimaryKey)

changeEmail :: PrimaryKey -> String -> ServerEffect Unit
changeEmail userID email = SD.execute (Query "update users set email = $2 where id = $1") (userID /\ email)


