module Client.Settings.Account where

import Prelude
import Shared.Types

import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Notification as CCN
import Data.String as DS
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Routes (routes)

update :: AffUpdate SettingsModel SettingsMessage
update { model, message } =
      case message of
            SetSField setter -> pure setter
            ChangeEmail -> changeEmail model
            ChangePassword -> changePassword model
            ToggleTerminateAccount -> toggleTerminateAccount model
            TerminateAccount -> terminateAccount

toggleTerminateAccount :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
toggleTerminateAccount _ = pure (\model -> model { confirmTermination = not model.confirmTermination})

changeEmail :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changeEmail model@({ email, emailConfirmation }) = do
      void $ request.settings.account.email { body: email }
      liftEffect $ CCN.alert "Email changed"
      FAE.diff {
            email: "",
            emailConfirmation: ""
      }

changePassword :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changePassword model@({ password, passwordConfirmation }) = do
      void $ request.settings.account.password { body: password }
      liftEffect do
            CCN.alert "Password changed, you will be logged out"
            liftEffect <<< CCL.setLocation $ routes.login.get {}
      FAE.noChanges

terminateAccount :: Aff (SettingsModel -> SettingsModel)
terminateAccount = do
      void $ request.settings.account.terminate { body:{} }
      liftEffect <<< CCL.setLocation $ routes.landing {}
      FAE.noChanges