module Server.Logout.Database where

import Droplet.Language
import Prelude
import Server.Database.Tokens

import Server.Database as SD
import Server.Database.Fields (_contents)
import Server.Effect (ServerEffect)

deleteToken ∷ Int → String → ServerEffect Unit
deleteToken loggedUserId loggedUserToken = SD.execute $ delete # from tokens # wher (_toker .=. loggedUserId .&&. _contents .=. loggedUserToken)