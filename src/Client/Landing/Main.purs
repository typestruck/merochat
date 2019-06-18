module Client.Landing.Main where

import Client.Common as C
import Effect (Effect)
import Prelude
import Data.Maybe(Maybe(..))
import Data.Maybe as M
import Type.Data.Boolean (kind Boolean)
import Web.UIEvent.MouseEvent.EventTypes (click)
import Effect.Class (liftEffect)
import Effect.Aff as A
import Shared.Types (RegisterLogin(..), Route(..))
import Client.Common.External as E
import Shared.Routing as R

foreign import grecaptchaExecute :: Effect Unit
foreign import grecaptchaReset :: Effect Unit

register :: Maybe String -> Effect Unit
register captchaResponse = do
	registerLogin <- E.validateEmailPassword
	case registerLogin of
		Nothing -> pure unit
	 	Just (RegisterLogin rl) ->
			if M.isNothing captchaResponse then
				grecaptchaExecute
			 else
				A.launchAff_ $ C.post (R.fromRoute Register) (RegisterLogin $ rl { captchaResponse = captchaResponse }) enter (const (liftEffect grecaptchaReset))

	where   enter token = liftEffect <<< E.login token $ R.fromRoute IM

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

main :: Effect Unit
main = do
	registerButton <- C.querySelector "#register"
	C.addEventListener registerButton click (const (register Nothing))
