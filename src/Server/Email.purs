module Server.Email where

import Prelude

import Server.Types (ServerEffect)

sendEmail :: String -> String -> ServerEffect Unit
sendEmail to content = do
