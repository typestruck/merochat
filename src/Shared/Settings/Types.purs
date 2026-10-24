module Shared.Settings.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADG
import Data.Argonaut.Encode.Generic as DAEG
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Shared.Modal (Modal)
import Shared.Network (RequestStatus)
import Shared.SuggestionsFrom (SuggestionsFrom)
import Shared.User (ProfileVisibility)

type SM =
      ( email ∷ String
      , emailConfirmation ∷ String
      , password ∷ String
      , erroredFields ∷ Array String
      , passwordConfirmation ∷ String
      , tab ∷ Tab
      , visible ∷ Boolean
      , requestStatus ∷ Maybe SettingsRequestStatus
      , accountRequest ∷ Maybe { field ∷ String, status ∷ RequestStatus }
      , accountLoading ∷ Boolean
      , confirmTermination ∷ Boolean
      | US
      )

type US = (chatBackground ∷ Maybe String, ownBackground ∷ Boolean | PS)

type UserSettings = Record US

type PS =
      ( readReceipts ∷ Boolean
      , typingStatus ∷ Boolean
      , profileVisibility ∷ ProfileVisibility
      , asksVisibility ∷ ProfileVisibility
      , postsVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Boolean
      , messageTimestamps ∷ Boolean
      , lastMessageOnContactList ∷ Boolean
      , suggestionsFrom ∷ SuggestionsFrom
      )

type PrivacySettings = Record PS

type SettingsModel = Record SM

type SettingsRequestStatus =
      { request ∷ SettingRequest
      , status ∷ RequestStatus
      }

data SettingRequest
      = RequestSavePrivacy
      | RequestSaveSuggestions
      | RequestSaveChatBackground

data SettingsMessage
      = SetSField (SettingsModel → SettingsModel)
      | ChangeEmail
      | ChangePrivacySettings SettingRequest
      | ShowSuccess SettingRequest
      | ChangePassword
      | SelectChatBackground
      | AfterAccountRequest String RequestStatus
      | SetChatBackground (Maybe String)
      | RemoveChatBackground
      | SaveChatBackground
      | AfterSaveChatBackground (Maybe String)
      | SelectChoice String (Maybe String) (Array { value ∷ String, name ∷ String }) (String → SettingsMessage)
      | ToggleVisibility Modal
      | ToggleTerminateAccount
      | TerminateAccount --very bad

data Tab
      = Privacy
      | Chats
      | Account

derive instance Eq Tab

derive instance Generic Tab _

instance DecodeJson Tab where
      decodeJson = DADG.genericDecodeJson

instance EncodeJson Tab where
      encodeJson = DAEG.genericEncodeJson

derive instance Eq SettingRequest

derive instance Generic SettingRequest _

instance DecodeJson SettingRequest where
      decodeJson = DADG.genericDecodeJson

instance EncodeJson SettingRequest where
      encodeJson = DAEG.genericEncodeJson
