module Client.Landing.Main where

import Prelude

import Client.Account as CCA
import Client.Dom as CCD
import Client.Location as CCL
import Client.Network (routes)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Routes (routesSpec)
import Shared.Unsafe as SU
import Web.HTML.Event.EventTypes (click)

registerRegularUser ∷ Effect Unit
registerRegularUser = do
      registerLogin ← CCA.validateEmailPassword
      case registerLogin of
            Nothing → pure unit
            Just rl → EA.launchAff_ do
                  status ← CCA.formRequest $ routes.register { body: { email: rl.email, password: rl.password, captchaResponse: "" } }
                  liftEffect $ case status of
                        Success → CCL.setLocation $ routesSpec.im.get {}
                        Failure _ → pure unit

registerTemporaryUser ∷ Effect Unit
registerTemporaryUser = EA.launchAff_ do
      status ← CCA.formRequest $ routes.temporary {}
      liftEffect $ case status of
            Success → CCL.setLocation $ routesSpec.im.get {}
            Failure _ → pure unit

registerTemporaryUserEvents ∷ Effect Unit
registerTemporaryUserEvents = do
      temporaryUserElement ← CCD.getElementById TemporaryUserSignUp
      CCD.addEventListener (SU.fromJust temporaryUserElement) click (const registerTemporaryUser)

main ∷ Effect Unit
main = do
      CCA.registerEvents registerRegularUser
      registerTemporaryUserEvents
