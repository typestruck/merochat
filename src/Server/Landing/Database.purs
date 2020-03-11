module Server.Landing.Database where

import Server.Types
import Shared.Types
import Server.Database as SD
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..))

createUser :: {email :: String, name :: String, password :: String, headline :: String, description :: String} -> ServerEffect PrimaryKey
createUser user = SD.insert (Query """INSERT INTO users(name, password, email, headline, description) VALUES ($1, $2, $3, $4, $5)""") (user.name /\ user.password /\ user.email /\ user.headline /\ user.description)