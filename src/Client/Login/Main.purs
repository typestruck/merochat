module Client.Login.Main where

import Prelude

import Client.Account as CCA
import Client.Dom as CCD
import Client.Location as CCL
import Client.Network (request)
import Client.Network as CCNT
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Network (RequestStatus(..))
import Shared.Routes (routes)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as WUK

login ∷ Effect Unit
login = do
      maybeRegisterLogin ← CCA.validateEmailPassword
      case maybeRegisterLogin of
            Nothing → pure unit
            Just registerLogin →
                  EA.launchAff_ do
                        status ← CCA.formRequest $ request.login.post { body: registerLogin }
                        liftEffect $ when (status == Success) do
                              -- the location to go after login is either the query parameter next or /im
                              redirect ← CCL.queryParameter "next"
                              CCL.setLocation $ DM.fromMaybe (routes.im.get {}) redirect

loginOnEnter ∷ Event → Effect Unit
loginOnEnter event = do
      let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
      when (pressed == "Enter") login

main ∷ Effect Unit
main = CCA.registerEvents login
