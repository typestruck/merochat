module Server.Logout.Database where

import Prelude

import Droplet.Language
import Server.Database as SD
import Server.Database.Tokens
import Server.Effect (ServerEffect)

logout :: Int -> ServerEffect Unit
logout loggedUserId = SD.execute $ delete # from tokens # wher (_toker .=. loggedUserId)