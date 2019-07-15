module Server.Database.Action where

import Server.Types
import Shared.Types
import Server.Database as SD
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..))

createUser :: {email :: String,	name :: String, password :: String, headline :: String, description :: String} -> ServerEffect PrimaryKey
createUser user = SD.insert (Query """INSERT INTO users(
	name, password, email, headline, description)
	VALUES (?, ?, ?, ?, ?)""") (user.name /\ user.password /\ user.headline /\ user.description)