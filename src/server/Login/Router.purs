module Server.Login.Router where

import Prelude
import Server.Login.Action as SLI
import Server.Login.Template as SLIT
import Server.Types
import Server.Router.Session as SRS
import Server.Response as SRR
import HTTPure (Method(..), Request, ResponseM, Path)

login :: Request -> ResponseEffect
login { method, body } = do
      SRS.checkAnonymous
     -- if method == Get then
      SRR.serveTemplate SLIT.template
      --  else
      --       SRR.json body SLI.login