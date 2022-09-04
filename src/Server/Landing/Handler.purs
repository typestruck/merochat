module Server.Landing.Handler where

import Prelude
import Server.Ok

import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Server.Response as SR
import Server.Types (ServerEffect)
import Shared.Account (RegisterLogin)
import Shared.Html (Html)

landing ∷ ∀ r. { | r } → ServerEffect Html
landing _ = SR.serveTemplate SLT.template

register ∷ ∀ r. { body ∷ RegisterLogin | r } → ServerEffect (Response Ok)
register { body } = do
      token ← SLA.register body
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok ok
