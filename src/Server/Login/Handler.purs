module Server.Login.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Either as DE
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Run.Reader as RR
import Server.Cookies as SC
import Server.Login.Action as SLA
import Server.Login.Template as SLT
import Server.Ok (ok)
import Server.Response as SR

login :: forall r. { | r } -> ServerEffect Html
login _ = SR.serveTemplate SLT.template

logon :: forall r. {body :: RegisterLogin | r } -> ServerEffect (Response Ok)
logon { body } = do
      token <- SLA.login body
      cookieHeader <- SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [cookieHeader]) $ PSR.ok ok