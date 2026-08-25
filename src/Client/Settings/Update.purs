module Client.Settings.Update where

import Prelude
import Shared.Im.Types

import Client.AppId (imAppId, settingsAppId)
import Client.File as CCF
import Client.File as CF
import Client.Location as CCL
import Client.Network (routes)
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
import Shared.Modal (ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Resource (maxImageSize)
import Shared.Routes (routesSpec)
import Shared.Settings.Types (SettingRequest(..), SettingsMessage(..), SettingsModel, PrivacySettings)
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
            ShowSuccess operation → showSuccess operation model
            BeforeSetChatBackground event → beforeSetChatBackground event model
            SetChatBackground image → setChatBackground image model
            AfterSaveChatBackground url → afterSaveChatBackground url model
            TerminateAccount → terminateAccount model
            RemoveChatBackground → removeChatBackground model
            SaveChatBackground → saveChatBackground model
            ChangePrivacySettings request → changePrivacySettings request model
            ToggleVisibility modal → setIt (_ { visible = modal == ShowSettings }) model

setIt ∷ (SettingsModel → SettingsModel) → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setIt s model = s model /\ []

changePrivacySettings ∷ SettingRequest → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePrivacySettings request model = model { requestStatus = Nothing } /\ [ change ]
      where
      change = do
            result ← CNN.request $ routes.settings.account.privacy { body: privacySettings model }
            case result of
                  Right _ → pure <<< Just $ ShowSuccess request
                  Left _ → pure <<< Just <<< SetSField $ _ { requestStatus = Just $ { request, status: Failure "" } }

showSuccess ∷ SettingRequest → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
showSuccess request model = model { requestStatus = Just { request, status: Success } } /\ [ sendHide ]
      where
      sendHide = do
            --let im know that the settings has changed
            EC.liftEffect <<< FS.send imAppId <<< SetPrivacySettings $ privacySettings model
            EA.delay $ Milliseconds 3000.0
            pure <<< Just <<< SetSField $ _ { requestStatus = Nothing }

privacySettings :: SettingsModel -> PrivacySettings
privacySettings model =
            { asksVisibility: model.asksVisibility
            , postsVisibility: model.postsVisibility
            , profileVisibility: model.profileVisibility
            , readReceipts: model.readReceipts
            , typingStatus: model.typingStatus
            , onlineStatus: model.onlineStatus
            , messageTimestamps: model.messageTimestamps
            , lastMessageOnContactList: model.lastMessageOnContactList
            , suggestionsFrom: model.suggestionsFrom
            }

toggleTerminateAccount ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
toggleTerminateAccount model = model { confirmTermination = not model.confirmTermination } /\ []

changeEmail ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changeEmail model = model /\ [ requestAndLogout (Proxy ∷ Proxy "email") $ routes.settings.account.email { body: { email: model.email } } ]

changePassword ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePassword model = model /\ [ requestAndLogout (Proxy ∷ Proxy "password") $ routes.settings.account.password { body: { password: model.password } } ]

requestAndLogout ∷ ∀ v field. IsSymbol field ⇒ Proxy field → Aff (ClientResponse v) → Aff (Maybe SettingsMessage)
requestAndLogout field aff = do
      status ← CNN.formRequest (SSV.formId field) aff
      when (status == Success) $ do
            EA.delay $ Milliseconds 3000.0
            EC.liftEffect <<< CCL.setLocation $ routesSpec.login.get {}
      pure Nothing

terminateAccount ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
terminateAccount model = model /\ [ requestAndLogout (Proxy ∷ Proxy "confirmTermination") $ routes.settings.account.terminate { body: {} } ]

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
saveChatBackground model = model { requestStatus = Nothing } /\ [ save ]
      where
      save = do
            response ← routes.settings.chat.background { body: { ownBackground: model.ownBackground, image: model.chatBackground } }
            case response of
                  Right (Response { body: url }) → pure <<< Just <<< AfterSaveChatBackground $ if DS.null url then Nothing else Just url
                  Left _ → pure <<< Just <<< SetSField $ _ { requestStatus = Just $ { request: RequestSaveChatBackground, status: Failure "" } }

afterSaveChatBackground ∷ Maybe String → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
afterSaveChatBackground url model = model { chatBackground = url } /\ [ success ]
      where
      success = do
            EC.liftEffect $ FS.send imAppId $ SetChatBackgroundFromProfile model.ownBackground url
            pure $ Just $ ShowSuccess $ RequestSaveChatBackground
