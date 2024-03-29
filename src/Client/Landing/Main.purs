module Client.Landing.Main where

import Prelude

import Client.Common.Account as CCA
import Client.Common.Captcha as CCC
import Client.Common.Dom as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Routes (routes)
import Shared.Unsafe as SU
import Web.HTML.Event.EventTypes (click)

registerRegularUser ∷ Maybe String → Effect Unit
registerRegularUser captchaResponse = do
      registerLogin ← CCA.validateEmailPassword
      case registerLogin of
            Nothing → pure unit
            Just rl → runCaptcha (DN.notNull "0") captchaResponse $ request.register { body: rl { captchaResponse = captchaResponse } }

registerTemporaryUser ∷ Maybe String → Effect Unit
registerTemporaryUser captchaResponse = runCaptcha (DN.notNull "1") captchaResponse $ request.temporary { body: { captchaResponse } }

runCaptcha ∷ Nullable String → Maybe String → Aff _ → Effect Unit
runCaptcha widgetId captchaResponse request = case captchaResponse of
      Nothing → CCC.execute widgetId
      _ → EA.launchAff_ do
            status ← CCA.formRequest request
            liftEffect $ case status of
                  Success → CCL.setLocation $ routes.im.get {}
                  Failure _ → CCC.reset widgetId

registerTemporaryUserEvents ∷ Effect Unit
registerTemporaryUserEvents = do
      temporaryUserElement ← CCD.getElementById TemporaryUserSignUp
      CCD.addEventListener (SU.fromJust temporaryUserElement) click (const (registerTemporaryUser Nothing))

initCaptchas ∷ Effect Unit
initCaptchas = do
      CCC.render (show CaptchaRegularUser) (CCC.defaultParameters (registerRegularUser <<< Just)) false
      CCC.render (show CaptchaTemporaryUser) (CCC.defaultParameters (registerTemporaryUser <<< Just)) false

main ∷ Effect Unit
main = do
      CCA.registerEvents (registerRegularUser Nothing)
      registerTemporaryUserEvents
