module Client.Recover.Main where

import Prelude

import Client.Common.Account as CCA
import Client.Common.Captcha as CCC
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Types (RequestStatus(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Routes (routes)

recover :: Maybe String -> Effect Unit
recover captchaResponse = do
     registerLogin <- CCA.validateEmail
     case registerLogin of
            Nothing -> pure unit
            Just email ->
                  if DM.isNothing captchaResponse then
                        CCC.grecaptchaExecute
                   else EA.launchAff_ do
                        status <- CCA.formRequest $ request.recover.post {body: { email, captchaResponse } }
                        liftEffect $ when (status == Fail) CCC.grecaptchaReset

-- | Callback for grecaptcha
completeRecover :: String -> Effect Unit
completeRecover captchaResponse = recover $ Just captchaResponse

reset :: String -> Effect Unit
reset token = do
      registerLogin <- CCA.validatePassword
      case registerLogin of
            Nothing -> pure unit
            Just password ->
                  EA.launchAff_ do
                        status <- CCA.formRequest $ request.recover.reset { body: { token, password } }
                        when (status == Success) do
                              EA.delay $ Milliseconds 3000.0
                              liftEffect <<< CCL.setLocation $ routes.login.get {}

main :: Effect Unit
main = do
      parameter <- CCL.queryParameter "token"
      case parameter of
            Nothing -> do
                  CCA.registerEvents (recover Nothing)
            Just token -> do
                  CCA.registerEvents (reset token)

