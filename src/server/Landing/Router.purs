module Server.Landing.Router where

import Prelude
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Server.Types
import Server.Router.Session as SRS
import HTTPure (Method(..), Request, ResponseM, Path)
import Server.Response as SRR

landing :: ResponseEffect
landing = do
        SRS.checkAnonymous
        SRR.serveTemplate SLT.template

register :: Request -> ResponseEffect
register { body } = do
      SRS.checkAnonymous
      SRR.serveTemplate SLT.template
      --SRR.json body $ SLA.register ""