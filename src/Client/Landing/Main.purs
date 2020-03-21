module Client.Landing.Main where

import Prelude

import Client.Common as CC
import Client.Common.External as CCE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Affjax as A
import Effect.Class (liftEffect)
import Shared.Unsafe as SU
import Data.Either (Either(..))
import Shared.Routing as SR
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
                                EA.launchAff_ $ do
                                        response <- CC.post (SR.fromRouteAbsolute Register) (RegisterLogin $ rl { captchaResponse = captchaResponse })
                                        case response of
                                                Right token -> enter token
                                                Left left -> liftEffect $ do
                                                        grecaptchaReset
                                                        EC.log $ A.printResponseFormatError left
                                                        CC.alert "Could not register. Please try again."
        where   enter token = liftEffect <<< CCE.login token $ SR.fromRouteAbsolute IM

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

registerOnEnter event = do
        let pressed = WUK.key <<< SU.unsafeFromJust "registerOnEnter" $ WUK.fromEvent event
        when (pressed == "Enter") $ register Nothing

main :: Effect Unit
main = do
        registerButton <- CC.querySelector "#register"
        signUpDiv <- CC.querySelector ".sign-up"
        CC.addEventListener signUpDiv keyup registerOnEnter
        CC.addEventListener registerButton click (const (register Nothing))
