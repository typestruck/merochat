module Client.Landing.Main where

import Prelude
import Shared.Types

import Client.Common.Captcha as CCC
import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.MouseEvent.EventTypes (click)

register :: Maybe String -> Effect Unit
register captchaResponse = do
      registerLogin <- CCE.validateEmailPassword
      case registerLogin of
            Nothing -> pure unit
            Just (RegisterLogin rl) ->
                  if DM.isNothing captchaResponse then
                        CCC.grecaptchaExecute
                   else
                        EA.launchAff_ do
                              response <- CCNT.post Register <<< Just <<< RegisterLogin $ rl { captchaResponse = captchaResponse }
                              case response of
                                    Right token -> enter token
                                    Left left -> liftEffect do
                                          CCC.grecaptchaReset
                                          CCN.alert left
      where enter token = liftEffect $ CCE.login token IM

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

registerOnEnter :: Event -> Effect Unit
registerOnEnter event = do
      let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
      when (pressed == "Enter") $ register Nothing

main :: Effect Unit
main = do
      registerButton <- CCD.querySelector "#register"
      signUpDiv <- CCD.querySelector ".form-up"
      CCD.addEventListener signUpDiv keyup registerOnEnter
      CCD.addEventListener registerButton click (const (register Nothing))
