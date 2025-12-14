module Client.Recover.Main where

import Prelude

import Client.Account as CCA
import Client.Location as CCL
import Client.Network (routes)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Network (RequestStatus(..))
import Shared.Routes (routesSpec)

recover ∷ Effect Unit
recover = do
      inputed ← CCA.validateEmail
      case inputed of
            Nothing → pure unit
            Just email → EA.launchAff_ <<< void <<< CCA.formRequest $ routes.recover.post { body: { email, captchaResponse: "" } }

reset ∷ String → Effect Unit
reset token = do
      passwordInputed ← CCA.validatePassword
      passwordConfirmationInputed ← CCA.validatePasswordConfirmation passwordInputed
      case passwordConfirmationInputed of
            Just password →
                  EA.launchAff_ do
                        status ← CCA.formRequest $ routes.recover.reset { body: { token, password } }
                        when (status == Success) do
                              EA.delay $ Milliseconds 3000.0
                              liftEffect <<< CCL.setLocation $ routesSpec.login.get {}
            _ → pure unit

main ∷ Effect Unit
main = do
      parameter ← CCL.queryParameter "token"
      case parameter of
            Nothing → do
                  CCA.registerEvents recover
            Just token → do
                  CCA.registerEvents (reset token)
