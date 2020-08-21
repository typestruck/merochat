module Client.Login.Main where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Client.Common.Location as CCL
import Client.Common.Network as CCNT
import Client.Common.Network (request)
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame.Types (NodeData(..))
import Payload.Client as PC
import Payload.ResponseTypes (Empty(..))

import Shared.Spec (spec)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.MouseEvent.EventTypes (click)

login :: Effect Unit
login = do
      maybeRegisterLogin  <- CCE.validateEmailPassword
      case maybeRegisterLogin of
            Nothing -> pure unit
            Just registerLogin -> EA.launchAff_ do
                  void <<< CCNT.response $ request.login.post { body: registerLogin }
                  liftEffect do
                        -- the location to go after login is either the query parameter next or /im
                        redirect <- CCL.search
                        pure unit
                        --CCL.setLocation $ DE.either (const IM) identity redirect

loginOnEnter :: Event -> Effect Unit
loginOnEnter event = do
      let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
      when (pressed == "Enter") login

main :: Effect Unit
main = do
      loginButton <- CCD.querySelector "#login"
      signUpDiv <- CCD.querySelector ".form-up"
      CCD.addEventListener signUpDiv keyup loginOnEnter
      CCD.addEventListener loginButton click (const login)
