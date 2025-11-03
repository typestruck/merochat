module Server.Landing.Handler where

import Prelude

import Effect.Class as EC
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Effect (ServerEffect)
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Shared.Account (EmailPasswordCaptcha)
import Shared.Html (Html)

landing ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect Html
landing _ = EC.liftEffect SLT.template

register ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPasswordCaptcha } → ServerEffect (Response Empty)
register { body } = do
      token ← SLA.registerRegularUser body
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok Empty

temporary ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect (Response Empty)
temporary _ = do
      token ← SLA.registerTemporaryUser
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok Empty
