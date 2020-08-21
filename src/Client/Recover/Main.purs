module Client.Recover.Main where

import Prelude
import Shared.Types

import Client.Common.Captcha as CCC
import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Class.Console as EC
import Shared.Routes (routes)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent.EventTypes (click)

recover :: Maybe String -> Effect Unit
recover captchaResponse = do
      emailElement <- CCD.querySelector "#email"
      email <- DS.trim <$> CCD.value emailElement
      if DS.null email then do
            CCN.alert "Email is mandatory"
       else if DM.isNothing captchaResponse then
            CCC.grecaptchaExecute
       else
            EA.launchAff_ do
                  response <- request.recover.post {body: { email, captchaResponse } }
                  case response of
                        Right _ -> liftEffect <<< CCN.alert $ "Recover link sent to " <> email
                        Left left -> liftEffect do
                              CCC.grecaptchaReset
                              CCN.alert "Could not recover. Please try again."

-- | Callback for grecaptcha
completeRecover :: String -> Effect Unit
completeRecover captchaResponse = recover $ Just captchaResponse

reset :: String -> Effect Unit
reset token = do
      passwordElement <- CCD.querySelector "#password"
      confirmPasswordElement <- CCD.querySelector "#confirm-password"
      password <- CCD.value passwordElement
      confirmPassword <- CCD.value confirmPasswordElement
      if DS.null password || DS.null confirmPassword then
            CCN.alert "Fill in password and confirmation"
       else if password /= confirmPassword then
            CCN.alert "Password and confirmation do not match"
       else  EA.launchAff_   do
            void $ request.recover.reset { body: { token, password } }
            liftEffect do
                  CCN.alert "Password reset! Redirecting to login"
                  CCL.setLocation $ routes.login.get {}

main :: Effect Unit
main = do
      formUpDiv <- CCD.querySelector ".form-up"
      parameter <- CCL.queryParameter "token"
      case parameter of
            Nothing -> do
                  recoverButton <- CCD.querySelector "#recover"
                  CCD.onEnter formUpDiv (recover Nothing)
                  CCD.addEventListener recoverButton click (const (recover Nothing))
            Just token -> do
                  resetButton <- CCD.querySelector "#reset"
                  CCD.onEnter formUpDiv (reset token)
                  CCD.addEventListener resetButton click (const (reset token))

