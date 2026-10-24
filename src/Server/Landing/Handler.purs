module Server.Landing.Handler where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class as EC
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Effect (ServerEffect)
import Server.Im.Action as SIA
import Server.Landing.Action as SLA
import Server.Landing.Template as SLT
import Shared.Account (EmailPasswordCaptcha)
import Shared.Html (Html)
import Shared.Login.Types (LoginResponse)

landing ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect Html
landing _ = EC.liftEffect SLT.template

register ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPasswordCaptcha, query ∷ { boot ∷ Boolean } } → ServerEffect (Response LoginResponse)
register { body, query: { boot } } = SLA.registerRegularUser body >>= registered boot

temporary ∷ { guards ∷ { checkAnonymous ∷ Unit }, query ∷ { boot ∷ Boolean } } → ServerEffect (Response LoginResponse)
temporary { query: { boot } } = SLA.registerTemporaryUser >>= registered boot

registered ∷ Boolean → { token ∷ String, userId ∷ Int } → ServerEffect (Response LoginResponse)
registered boot { token, userId } = do
      payload ←
            if boot then do
                  { user, contacts, suggestions } ← SIA.im userId
                  pure { user: Just user, contacts, suggestions }
            else pure { user: Nothing, contacts: [], suggestions: [] }
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok payload
