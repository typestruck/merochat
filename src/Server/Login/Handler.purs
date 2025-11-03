module Server.Login.Handler where

import Prelude

import Effect.Class as EC
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Effect (ServerEffect)
import Server.Login.Action as SLA
import Server.Login.Template as SLT
import Shared.Account (EmailPassword)
import Shared.Html (Html)

login ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect Html
login _ = EC.liftEffect SLT.template

logon ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPassword } → ServerEffect (Response Empty)
logon { body } = do
      token ← SLA.login body
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok Empty