module Client.Recover.Main where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Router as SR
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.MouseEvent.EventTypes (click)

foreign import grecaptchaExecute :: Effect Unit
foreign import grecaptchaReset :: Effect Unit

recover :: Maybe String -> Effect Unit
recover captchaResponse = do
      emailElement <- CCD.querySelector "#email"
      email <- CCD.value emailElement
      if DS.null email then do
            CCN.alert "Email is mandatory"
       else if DM.isNothing captchaResponse then
            grecaptchaExecute
       else
            EA.launchAff_ do
                  response <- CCNT.post (Recover { token: Nothing }) <<< Just $ RecoverPassword {
                        email: email,
                        captchaResponse: captchaResponse
                  }
                  case response of
                        Right Ok -> liftEffect <<< CCN.alert $ "Recover link sent to" <> email
                        Left left -> liftEffect do
                              grecaptchaReset
                              CCN.alert "Could not recover. Please try again."

-- | Callback for grecaptcha
completeRecover :: String -> Effect Unit
completeRecover captchaResponse = recover $ Just captchaResponse

recoverOnEnter :: Event -> Effect Unit
recoverOnEnter event = do
      let pressed = WUK.key <<< SU.fromJust "recover" $ WUK.fromEvent event
      when (pressed == "Enter") $ recover Nothing

main :: Effect Unit
main = do
      recoverButton <- CCD.querySelector "#login"
      signUpDiv <- CCD.querySelector ".form-up"
      CCD.addEventListener signUpDiv keyup recoverOnEnter
      CCD.addEventListener recoverButton click (const (recover Nothing))
