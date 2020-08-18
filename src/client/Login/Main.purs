module Client.Login.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Data.Maybe as DM
import Effect (Effect)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.MouseEvent.EventTypes (click)

login :: Event -> Effect Unit
login event = do
      maybeRegisterLogin <- CCE.validateEmailPassword
      when (DM.isNothing maybeRegisterLogin) $ CCD.preventStop event

loginOnEnter :: Event -> Effect Unit
loginOnEnter event = do
      let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
      when (pressed == "Enter") $ login event

main :: Effect Unit
main = do
      loginButton <- CCD.querySelector "#login"
      signUpDiv <- CCD.querySelector ".form-up"
      CCD.addEventListener signUpDiv keyup loginOnEnter
      CCD.addEventListener loginButton click login
