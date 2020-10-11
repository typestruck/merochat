module Test.Server.User where

import Database.PostgreSQL (Query(..), Row0(..))
import Server.Database as SD
import Server.Types

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

email :: String
email = "e@a.com"

password :: String
password = "hunter12"