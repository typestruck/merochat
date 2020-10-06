module Client.Landing.Main where

import Prelude

import Client.Common.Account as CCA
import Client.Common.Captcha as CCC
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Types (RequestStatus(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Routes (routes)

register :: Maybe String -> Effect Unit
register captchaResponse = do
      registerLogin <- CCA.validateEmailPassword
      case registerLogin of
            Nothing -> pure unit
            Just rl ->
                  if DM.isNothing captchaResponse then
                        CCC.grecaptchaExecute
                   else EA.launchAff_ do
                        status <- CCA.formRequest $ request.register $ { body: rl { captchaResponse = captchaResponse }}
                        liftEffect $ case status of
                              Success -> CCL.setLocation $ routes.im.get {}
                              Fail -> CCC.grecaptchaReset

-- | Callback for grecaptcha
completeRegistration :: String -> Effect Unit
completeRegistration captchaResponse = register $ Just captchaResponse

main :: Effect Unit
main = CCA.registerEvents (register Nothing)

