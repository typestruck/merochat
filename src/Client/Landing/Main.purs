module Client.Landing.Main where

import Prelude

import Client.Common.Account as CCA
import Client.Common.Dom as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Routes (routes)
import Shared.Unsafe as SU
import Web.HTML.Event.EventTypes (click)

registerRegularUser ∷ Effect Unit
registerRegularUser = do
      registerLogin ← CCA.validateEmailPassword
      case registerLogin of
            Nothing → pure unit
            Just rl → EA.launchAff_ do
                  status ← CCA.formRequest $ request.register { body:  { email : rl.email, password : rl.password, captchaResponse : "" } }
                  liftEffect $ case status of
                        Success → CCL.setLocation $ routes.im.get {}
                        Failure _ → pure unit

registerTemporaryUser ∷ Effect Unit
registerTemporaryUser  =  EA.launchAff_ do
      status ← CCA.formRequest $ request.temporary {  }
      liftEffect $ case status of
            Success → CCL.setLocation $ routes.im.get {}
            Failure _ → pure unit

registerTemporaryUserEvents ∷ Effect Unit
registerTemporaryUserEvents = do
      temporaryUserElement ← CCD.getElementById TemporaryUserSignUp
      CCD.addEventListener (SU.fromJust temporaryUserElement) click (const registerTemporaryUser)

main ∷ Effect Unit
main = do
      CCA.registerEvents registerRegularUser
      registerTemporaryUserEvents
