module Client.Landing.Main where

import Prelude

import Client.Common as CC
import Client.Common.External as CCE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import Shared.Routing as SR
import Shared.Types
import Web.UIEvent.MouseEvent.EventTypes (click)

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
                                                _ -> liftEffect grecaptchaReset
        where   enter token = liftEffect <<< CCE.login token $ SR.fromRouteAbsolute IM

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

main :: Effect Unit
main = do
        registerButton <- CC.querySelector "#register"
        CC.addEventListener registerButton click (const (register Nothing))
