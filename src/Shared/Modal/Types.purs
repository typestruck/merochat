module Shared.Modal.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)

data Modal = HideModal | Screen ScreenModal | Confirmation ConfirmationModal | Chat ChatModal | Special SpecialModal

data ScreenModal
      = ShowMenu
      | ShowExperiments
      | ShowProfile
      | ShowSettings
      | ShowKarmaPrivileges
      | ShowHelp
      | ShowBacker
      | ShowFeedback

data ConfirmationModal
      = ConfirmLogout
      | ConfirmTerminationTemporaryUser
      | ConfirmDeleteChat Int
      | ConfirmBlockUser Int
      | ConfirmReport Int

data SpecialModal
      = ShowSuggestionCard Int
      | Tutorial Step

data ChatModal
      = ShowSelectedImage
      | ShowAudioPrompt
      | ShowEmojis

data Step
      = Welcome
      | ChatSuggestions
      | Chatting
      | BackSuggestions
      | ChatList
      | OptionsMenu

derive instance Eq ConfirmationModal
derive instance Eq SpecialModal

instance DecodeJson ConfirmationModal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson SpecialModal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Modal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ScreenModal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Step where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ChatModal where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson ConfirmationModal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson SpecialModal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson Step where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson Modal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ScreenModal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ChatModal where
      encodeJson = DAEGR.genericEncodeJson

instance Show ScreenModal where
      show = case _ of
            ShowProfile → "Profile"
            ShowSettings → "Settings"
            ShowKarmaPrivileges → "Karma"
            ShowHelp → "Help"
            ShowExperiments → "Experiments"
            ShowBacker → "Support us"
            ShowFeedback → "Send feedback"
            _ → ""

derive instance Eq ChatModal
derive instance Eq Modal
derive instance Eq Step
derive instance Eq ScreenModal

derive instance Generic Step _
derive instance Generic ConfirmationModal _
derive instance Generic SpecialModal _
derive instance Generic ChatModal _
derive instance Generic Modal _
derive instance Generic ScreenModal _