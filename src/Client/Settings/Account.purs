module Client.Settings.Account where

import Prelude
import Shared.Im.Types

import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CNN
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Update)
import Flame.Subscription as FS
import Payload.Client (ClientResponse)
import Shared.Modal.Types (ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Client.AppId (imAppId)
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettingsId(..), SettingsMessage(..), SettingsModel)
import Shared.Settings.View as SSV
import Type.Proxy (Proxy(..))

update ∷ Update SettingsModel SettingsMessage
update model message =
      case message of
            SetSField s → setIt s model
            ChangeEmail → changeEmail model
            ChangePassword → changePassword model
            ToggleTerminateAccount → toggleTerminateAccount model
            ShowSuccess → showSuccess model
            TerminateAccount → terminateAccount model
            ChangePrivacySettings → changePrivacySettings model
            ToggleVisibility modal → setIt (_ { visible = modal == ShowSettings }) model

setIt ∷ (SettingsModel → SettingsModel) → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setIt s model = s model /\ []

changePrivacySettings ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePrivacySettings model = model /\ [ change ]
      where
      payload = { postsVisibility: model.postsVisibility, profileVisibility: model.profileVisibility, readReceipts: model.readReceipts, typingStatus: model.typingStatus, onlineStatus: model.onlineStatus, messageTimestamps: model.messageTimestamps }
      change = do
            status ← CNN.formRequest (show PrivacySettingsId) $ request.settings.account.privacy { body: payload }
            case status of
                  Success → do
                        --let im know that the settings has changed
                        EC.liftEffect <<< FS.send imAppId $ SetPrivacySettings payload
                        pure $ Just ShowSuccess
                  _ → pure Nothing

showSuccess ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
showSuccess model = model { hideSuccessMessage = false } /\ [ hide ]
      where
      hide = do
            EA.delay $ Milliseconds 3000.0
            pure <<< Just <<< SetSField $ _ { hideSuccessMessage = true }

toggleTerminateAccount ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
toggleTerminateAccount model = model { confirmTermination = not model.confirmTermination } /\ []

changeEmail ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changeEmail model = model /\ [ requestAndLogout (Proxy ∷ Proxy "email") $ request.settings.account.email { body: { email: model.email } } ]

changePassword ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePassword model = model /\ [ requestAndLogout (Proxy ∷ Proxy "password") $ request.settings.account.password { body: { password: model.password } } ]

requestAndLogout ∷ ∀ v field. IsSymbol field ⇒ Proxy field → Aff (ClientResponse v) → Aff (Maybe SettingsMessage)
requestAndLogout field aff = do
      status ← CNN.formRequest (SSV.formId field) aff
      when (status == Success) $ do
            EA.delay $ Milliseconds 3000.0
            EC.liftEffect <<< CCL.setLocation $ routes.login.get {}
      pure Nothing

terminateAccount ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
terminateAccount model = model /\ [ requestAndLogout (Proxy ∷ Proxy "confirmTermination") $ request.settings.account.terminate { body: {} } ]
