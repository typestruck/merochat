module Client.Settings.Update where

import Prelude

import Shared.Dropdown as SDRO
import Client.AppId (imAppId)
import Client.Lynx.Image as CLI
import Client.Lynx.Session as CLS
import Client.File as CF
import Client.Navigation as CNA
import Shared.External.Types as SET
import Client.Network (routes)
import Client.Network as CN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as DSY
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Update)
import Flame.Subscription as FS
import Payload.Client (ClientResponse)
import Payload.ResponseTypes (Response(..))
import Shared.Im.Types (ImMessage(SetChatBackgroundFromProfile, SetPrivacySettings))
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Resource (maxImageSize)
import Shared.Routes (routesSpec)
import Shared.Settings.Types (SettingRequest(..), SettingsMessage(..), SettingsModel, PrivacySettings)
import Shared.Network as SN
import Type.Proxy (Proxy(..))

update ∷ Update SettingsModel SettingsMessage
update model message =
      case message of
            SetSField s → setIt s model
            ChangeEmail → changeEmail model
            ChangePassword → changePassword model
            ToggleTerminateAccount → toggleTerminateAccount model
            ShowSuccess operation → showSuccess operation model
            SelectChatBackground → model /\ [ map (map $ SetChatBackground <<< Just) CLI.pick ]
            AfterAccountRequest field status → afterAccountRequest field status model
            SetChatBackground image → setChatBackground image model
            AfterSaveChatBackground url → afterSaveChatBackground url model
            TerminateAccount → terminateAccount model
            RemoveChatBackground → removeChatBackground model
            SaveChatBackground → saveChatBackground model
            ChangePrivacySettings request → changePrivacySettings request model
            SelectChoice title initial options select → model /\ [ map (map select) $ SDRO.pick title initial options ]
            ToggleVisibility modal → toggleVisibility model modal

setIt ∷ (SettingsModel → SettingsModel) → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setIt s model = s model /\ []

changePrivacySettings ∷ SettingRequest → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePrivacySettings request model = model { requestStatus = Nothing } /\ [ change ]
      where
      change = do
            result ← CN.request $ routes.settings.account.privacy { body: privacySettings model }
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

privacySettings ∷ SettingsModel → PrivacySettings
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
changeEmail model = requestAndLogout "email" (routes.settings.account.email { body: { email: model.email } }) model

changePassword ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
changePassword model = requestAndLogout "password" (routes.settings.account.password { body: { password: model.password } }) model

requestAndLogout ∷ ∀ value. String → Aff (ClientResponse value) → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
requestAndLogout field request model
      | model.accountLoading = model /\ []
      | otherwise = model { accountLoading = true, accountRequest = Nothing } /\ [ send ]
      where
      send = do
            result ← CN.request request
            pure <<< Just <<< AfterAccountRequest field $ case result of
                  Right _ → Success
                  Left error → Failure $ SN.errorMessage error

afterAccountRequest ∷ String → RequestStatus → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
afterAccountRequest field status model = model { accountLoading = status == Success, accountRequest = Just { field, status } } /\ effects
      where
      effects = case status of
            Failure _ → []
            Success → [ do
                  EA.delay $ Milliseconds 3000.0
                  EC.liftEffect do
                        CLS.clear
                        CNA.navigate SET.Login
                  pure Nothing
                  ]

terminateAccount ∷ SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
terminateAccount model = requestAndLogout "confirmTermination" (routes.settings.account.terminate { body: {} }) model

setChatBackground ∷ Maybe String → SettingsModel → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
setChatBackground image model =
      if isTooLarge $ DM.fromMaybe "" image then
            model
                  { erroredFields = [ DSY.reflectSymbol (Proxy ∷ Proxy "chatBackground") ]
                  } /\ []
      else
            model
                  { chatBackground = image
                  } /\ []
      where
      isTooLarge contents = maxImageSize < CF.fileSize contents

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

toggleVisibility ∷ SettingsModel → Modal → SettingsModel /\ Array (Aff (Maybe SettingsMessage))
toggleVisibility model = case _ of
      HideModal → model /\ [ hide ]
      Screen modal → model { visible = modal == ShowSettings } /\ []
      _ → model /\ []
      where
      hide = EC.liftEffect do
            FS.send imAppId <<< SIT.SpecialRequest $ SIT.ToggleModal HideModal
            pure Nothing
