module Server.Landing.Handler where

import Prelude
import Server.Ok

import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Effect (ServerEffect)
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Server.Response as SR
import Shared.Account (EmailPasswordCaptcha, RegisterTemporary)
import Shared.Html (Html)

landing ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect Html
landing _ = SR.serveTemplate SLT.template

register ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPasswordCaptcha } → ServerEffect (Response Ok)
register { body } = do
      token ← SLA.registerRegularUser body
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok ok

temporary ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect (Response Ok)
temporary _ = do
      token ← SLA.registerTemporaryUser
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok ok
