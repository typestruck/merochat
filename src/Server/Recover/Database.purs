module Server.Recover.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..))
import Server.Database as SD

--REFACTOR: consider making the uuid token the primary key
insertRecover :: PrimaryKey -> String -> ServerEffect Unit
insertRecover id token = void $ SD.insert (Query "insert into recoveries (uuid, recoverer) values($1, $2)") (token /\ id)

selectRecoverer :: String -> ServerEffect (Maybe PrimaryKey)
selectRecoverer token = SD.scalar (Query "select recoverer from recoveries where uuid = $1 and active = true and created >=  (now() at time zone 'utc') - interval '1 day'") $ Row1 token

recoverPassword :: String -> PrimaryKey -> String -> ServerEffect Unit
recoverPassword token id password = SD.withTransaction $ \connection -> do
      SD.executeWith connection (Query "update recoveries set active = false where uuid = $1") $ Row1 token
      SD.executeWith connection (Query "update users set password = $1 where id = $2") (password /\ id)