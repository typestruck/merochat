module Shared.Settings.Types where

import Prelude

import Shared.Modal.Types (ScreenModal)
import Shared.User (ProfileVisibility)

type SM =
      ( email ∷ String
      , emailConfirmation ∷ String
      , password ∷ String
      , erroredFields ∷ Array String
      , passwordConfirmation ∷ String
      , visible ∷ Boolean
      , hideSuccessMessage ∷ Boolean
      , confirmTermination ∷ Boolean
      | PS
      )

type PS =
      ( readReceipts ∷ Boolean
      , typingStatus ∷ Boolean
      , profileVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Boolean
      , messageTimestamps ∷ Boolean
      )

type PrivacySettings = Record PS

type SettingsModel = Record SM

data SettingsMessage
      = SetSField (SettingsModel → SettingsModel)
      | ChangeEmail
      | ChangePrivacySettings
      | ChangePassword
      | ToggleVisibility ScreenModal
      | ToggleTerminateAccount
      | TerminateAccount --very bad

data PrivacySettingsId = PrivacySettingsId

instance Show PrivacySettingsId where
      show _ = "privacy-settings"