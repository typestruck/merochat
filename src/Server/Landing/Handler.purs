module Server.Landing.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Tuple (Tuple(..))
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Run.Reader as RR
import Server.Cookies as SC
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Server.Ok (ok)
import Server.Response as SR

landing :: forall r. { | r } -> ServerEffect Html
landing _ = SR.serveTemplate SLT.template

register :: forall r. { body :: RegisterLogin | r } -> ServerEffect (Response Ok)
register { body } = do
      token <- SLA.register body
      cookieHeader <- SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [cookieHeader]) $ PSR.ok ok
