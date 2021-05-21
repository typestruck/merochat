module Server.Recover.Database where

import Prelude
import Server.Types
import Shared.Types
import Droplet.Language
import Server.Database.User
import Server.Database.Recoveries
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Server.Database as SD

--REFACTOR: consider making the uuid token the primary key
insertRecover :: Int -> String -> ServerEffect Unit
insertRecover id token = void $ SD.insert ("insert into recoveries (uuid, recoverer) values($1, $2)") (token /\ id)

selectRecoverer :: String -> ServerEffect (Maybe Int)
selectRecoverer token = SD.scalar ("select recoverer from recoveries where uuid = $1 and active = true and created >=  (now() at time zone 'utc') - interval '1 day'") $ Row1 token

recoverPassword :: String -> Int -> String -> ServerEffect Unit
recoverPassword token id password = SD.withTransaction $ \connection -> do
      SD.executeWith connection $ update recoveries # set (_active /\ false) # wher (_uuid .=. token)
      SD.executeWith connection $ update users # set (_password /\ password) 3 wher (_id .=. id)