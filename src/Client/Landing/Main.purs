module Client.Landing.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Client.Common.Notification as CCN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Web.Event.Internal.Types (Event)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Unsafe as SU
import Data.Either (Either(..))
import Shared.Router as SR
import Client.Common.Network as CCNT
import Shared.Types
import Web.UIEvent.MouseEvent.EventTypes (click)
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.KeyboardEvent as WUK

foreign import grecaptchaExecute :: Effect Unit
foreign import grecaptchaReset :: Effect Unit

register :: Maybe String -> Effect Unit
register captchaResponse = do
        registerLogin <- CCE.validateEmailPassword
        case registerLogin of
                Nothing -> pure unit
                Just (RegisterLogin rl) ->
                        if DM.isNothing captchaResponse then
                                grecaptchaExecute
                         else
                                EA.launchAff_ do
                                        response <- CCNT.post (SR.fromRoute Register) <<< Just <<< RegisterLogin $ rl { captchaResponse = captchaResponse }
                                        case response of
                                                Right token -> enter token
                                                Left left -> liftEffect do
                                                        grecaptchaReset
                                                        CCN.alert "Could not register. Please try again."
        where   enter token = liftEffect <<< CCE.login token $ SR.fromRoute IM

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

registerOnEnter :: Event -> Effect Unit
registerOnEnter event = do
        let pressed = WUK.key <<< SU.unsafeFromJust "registerOnEnter" $ WUK.fromEvent event
        when (pressed == "Enter") $ register Nothing

main :: Effect Unit
main = do
        registerButton <- CCD.querySelector "#register"
        signUpDiv <- CCD.querySelector ".sign-up"
        CCD.addEventListener signUpDiv keyup registerOnEnter
        CCD.addEventListener registerButton click (const (register Nothing))
