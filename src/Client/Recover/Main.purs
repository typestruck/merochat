module Client.Recover.Main where

import Prelude

import Client.Common.Account as CCA
import Client.Common.Captcha as CCC
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable (null)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Network (RequestStatus(..))
import Shared.Routes (routes)

recover ∷ Maybe String → Effect Unit
recover captchaResponse = do
      inputed ← CCA.validateEmail
      case inputed of
            Nothing → pure unit
            Just email →
                  if DM.isNothing captchaResponse then
                        CCC.execute null
                  else EA.launchAff_ do
                        status ← CCA.formRequest $ request.recover.post { body: { email, captchaResponse } }
                        liftEffect <<< unless (status == Success) $ CCC.reset null

-- | Callback for grecaptcha
completeRecover ∷ String → Effect Unit
completeRecover captchaResponse = recover $ Just captchaResponse

reset ∷ String → Effect Unit
reset token = do
      passwordInputed ← CCA.validatePassword
      passwordConfirmationInputed ← CCA.validatePasswordConfirmation passwordInputed
      case passwordConfirmationInputed of
            Just password →
                  EA.launchAff_ do
                        status ← CCA.formRequest $ request.recover.reset { body: { token, password } }
                        when (status == Success) do
                              EA.delay $ Milliseconds 3000.0
                              liftEffect <<< CCL.setLocation $ routes.login.get {}
            _ → pure unit

main ∷ Effect Unit
main = do
      parameter ← CCL.queryParameter "token"
      case parameter of
            Nothing → do
                  CCA.registerEvents (recover Nothing)
            Just token → do
                  CCA.registerEvents (reset token)
