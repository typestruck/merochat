module Client.Settings.Account where

import Prelude
import Shared.ContentType

import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CNN
import Client.Common.Types (RequestStatus(..))
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Payload.Client (ClientResponse)
import Shared.IM.Types (IMMessage(..))
import Shared.Options.MountPoint (imID)
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettingsId(..), SettingsMessage(..), SettingsModel)
import Shared.Settings.View as SSV
import Type.Proxy (Proxy(..))

update ∷ AffUpdate SettingsModel SettingsMessage
update w@{ model, message } =
      case message of
            SetSField setter → pure setter
            ChangeEmail → changeEmail model
            ChangePassword → changePassword model
            ToggleTerminateAccount → toggleTerminateAccount model
            TerminateAccount → terminateAccount
            ChangePrivacySettings → changePrivacySettings w

changePrivacySettings ∷ AffUpdate SettingsModel SettingsMessage
changePrivacySettings { display, model: { readReceipts, typingStatus, onlineStatus, messageTimestamps, profileVisibility } } = do
      status ← CNN.formRequest (show PrivacySettingsId) $ request.settings.account.privacy { body: { profileVisibility, readReceipts, typingStatus, onlineStatus, messageTimestamps } }
      case status of
            Success → do
                  display $ _ { hideSuccessMessage = false }
                  liftEffect <<<
                        --let im know that the settings has changed
                        FS.send imID $ SetPrivacySettings { profileVisibility, readReceipts, typingStatus, onlineStatus, messageTimestamps }
                  EA.delay $ Milliseconds 3000.0
                  FAE.diff { hideSuccessMessage: true }
            _ → FAE.noChanges

toggleTerminateAccount ∷ SettingsModel → Aff (SettingsModel → SettingsModel)
toggleTerminateAccount _ = pure (\model → model { confirmTermination = not model.confirmTermination })

changeEmail ∷ SettingsModel → Aff (SettingsModel → SettingsModel)
changeEmail { email } = requestAndLogout (Proxy ∷ Proxy "email") $ request.settings.account.email { body: { email } }

changePassword ∷ SettingsModel → Aff (SettingsModel → SettingsModel)
changePassword { password } = requestAndLogout (Proxy ∷ Proxy "password") $ request.settings.account.password { body: { password } }

requestAndLogout ∷ ∀ v field. IsSymbol field ⇒ Proxy field → Aff (ClientResponse v) → Aff (SettingsModel → SettingsModel)
requestAndLogout field aff = do
      status ← CNN.formRequest (SSV.formId field) aff
      when (status == Success) $ do
            EA.delay $ Milliseconds 3000.0
            liftEffect <<< CCL.setLocation $ routes.login.get {}
      FAE.noChanges

terminateAccount ∷ Aff (SettingsModel → SettingsModel)
terminateAccount = requestAndLogout (Proxy ∷ Proxy "confirmTermination") $ request.settings.account.terminate { body: {} }
