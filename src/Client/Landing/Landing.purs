module Client.Landing.Main where

import Client.Common as C
import Effect (Effect)
import Prelude
import Data.Maybe(Maybe(..))
import Client.External.RegisterLogin(Endpoint(..))
import Client.External.RegisterLogin as R
import Type.Data.Boolean (kind Boolean)
import Web.UIEvent.MouseEvent.EventTypes (click)

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = R.registerLogin Register false $ Just captchaResponse

main :: Effect Unit
main = do
	register <- C.querySelector "#register"
	C.addEventListener register click (const (R.registerLogin Register true Nothing))
