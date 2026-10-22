module Server.Login.Handler where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class as EC
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Effect (ServerEffect)
import Server.Im.Action as SIA
import Server.Login.Action as SLA
import Server.Login.Template as SLT
import Shared.Account (EmailPassword)
import Shared.Html (Html)
import Shared.Login.Types (LoginResponse)

login ∷ { guards ∷ { checkAnonymous ∷ Unit } } → ServerEffect Html
login _ = EC.liftEffect SLT.template

logon ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailPassword, query ∷ { boot ∷ Boolean } } → ServerEffect (Response LoginResponse)
logon { body, query: { boot } } = do
      { token, userId } ← SLA.login body
      payload ←
            if boot then do
                  { user, contacts, suggestions } ← SIA.im userId
                  pure { user: Just user, contacts, suggestions }
            else pure { user: Nothing, contacts: [], suggestions: [] }
      cookieHeader ← SC.makeCookieHeader token
      pure <<< PSR.setHeaders (PH.fromFoldable [ cookieHeader ]) $ PSR.ok payload
