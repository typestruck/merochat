module Shared.Settings.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Shared.Modal.Types (ScreenModal)
import Shared.User (ProfileVisibility)
import Web.Event.Internal.Types (Event)

type SM =
      ( email ∷ String
      , emailConfirmation ∷ String
      , password ∷ String
      , erroredFields ∷ Array String
      , passwordConfirmation ∷ String
      , tab :: Tab
      , visible ∷ Boolean
      , chatBackground :: Maybe String
      , hideSuccessMessage ∷ Boolean
      , confirmTermination ∷ Boolean
      | PS
      )

type PS =
      ( readReceipts ∷ Boolean
      , typingStatus ∷ Boolean
      , profileVisibility ∷ ProfileVisibility
      , postsVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Boolean
      , messageTimestamps ∷ Boolean
      )

type PrivacySettings = Record PS

type SettingsModel = Record SM

data SettingsMessage
      = SetSField (SettingsModel → SettingsModel)
      | ChangeEmail
      | ChangePrivacySettings
      | ShowSuccess
      | ChangePassword
      | BeforeSetChatBackground Event
      | SaveChatBackground
      | ToggleVisibility ScreenModal
      | ToggleTerminateAccount
      | TerminateAccount --very bad

data Tab =
      Privacy |
      Chats |
      Account

derive instance Eq Tab

derive instance Generic Tab _

instance DecodeJson Tab where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Tab where
      encodeJson = DAEGR.genericEncodeJson