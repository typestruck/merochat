module Server.Login.Handler where

import Prelude
import Server.Effect (ServerEffect)

import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Login.Action as SLA
import Server.Login.Template as SLT
import Server.Ok (Ok, ok)
import Server.Response as SR
import Shared.Account (EmailPassword)
import Shared.Html (Html)

login ∷  { guards ∷ { checkAnonymous ∷ Unit }  } → ServerEffect Html
login _ = SR.serveTemplate SLT.template

logon ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPassword  } → ServerEffect (Response Ok)
logon { body } = do
      token ← SLA.login body
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok ok