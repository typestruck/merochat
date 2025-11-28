module Client.Settings.Update where

import Prelude
import Shared.Im.Types

import Client.AppId (imAppId, settingsAppId)
import Client.File as CCF
import Client.File as CF
import Client.Location as CCL
import Client.Network (request)
import Client.Network as CNN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol (class IsSymbol)
import Data.Symbol as TDS
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Update)
import Flame.Subscription as FS
import Payload.Client (ClientResponse)
import Payload.ResponseTypes (Response(..))
import Shared.Element (ElementId(..))
import Shared.Modal (ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Resource (maxImageSize)
import Shared.Routes (routes)
import Shared.Settings.Types (SettingsMessage(..), SettingsModel)
import Shared.Settings.View as SSV
import Type.Proxy (Proxy(..))
import Web.Event.Internal.Types (Event)

update ∷ Update SettingsModel SettingsMessage
update model message =
      case message of
            SetSField s → setIt s model
            ChangeEmail → changeEmail model
            ChangePassword → changePassword model
            ToggleTerminateAccount → toggleTerminateAccount model
            ShowSuccess → showSuccess model
            BeforeSetChatBackground event → beforeSetChatBackground event model
            SetChatBackground image → setChatBackground image model
            AfterSaveChatBackground url → afterSaveChatBackground url model
            TerminateAccount → terminateAccount model
            RemoveChatBackground → removeChatBackground model
            SaveChatBackground → saveChatBackground model
            ChangePrivacySettings → changePrivacySettings model
            ToggleVisibility modal → setIt (_ { visible = modal == ShowSettings }) model

setIt ∷ (SettingsModel → SettingsModel) → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setIt s model = s model /\ []

changePrivacySettings ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePrivacySettings model = model /\ [ change ]
      where
      payload = { asksVisibility: model.asksVisibility, postsVisibility: model.postsVisibility, profileVisibility: model.profileVisibility, readReceipts: model.readReceipts, typingStatus: model.typingStatus, onlineStatus: model.onlineStatus, messageTimestamps: model.messageTimestamps }
      change = do
            status ← CNN.formRequest (show PrivacySettings) $ request.settings.account.privacy { body: payload }
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

beforeSetChatBackground ∷ Event → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
beforeSetChatBackground event model = model /\ [ before ]
      where
      before = do
            CF.compressImage settingsAppId event true (\_ _ b → SetChatBackground $ Just b)
            pure Nothing

setChatBackground ∷ Maybe String → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setChatBackground image model =
      if isTooLarge $ DM.fromMaybe "" image then
            model
                  { erroredFields = [ TDS.reflectSymbol (Proxy ∷ Proxy "chatBackground") ]
                  } /\ []
      else
            model
                  { chatBackground = image
                  } /\ []
      where
      isTooLarge contents = maxImageSize < CCF.fileSize contents

removeChatBackground ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
removeChatBackground model = model { chatBackground = Nothing } /\ [ save ]
      where
      save = pure $ Just SaveChatBackground

saveChatBackground ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
saveChatBackground model = model /\ [ save ]
      where
      save = do
            response ← request.settings.chat.background { body: { ownBackground: model.ownBackground, image: model.chatBackground } }
            case response of
                  Right (Response { body: url }) → pure <<< Just <<< AfterSaveChatBackground $ if DS.null url then Nothing else Just url
                  _ → pure Nothing

afterSaveChatBackground ∷ Maybe String → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
afterSaveChatBackground url model = model { chatBackground = url } /\ [ success, notify ]
      where
      success = pure $ Just ShowSuccess
      notify = EC.liftEffect do
            FS.send imAppId $ SetChatBackgroundFromProfile model.ownBackground url
            pure Nothing