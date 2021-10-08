module Shared.Settings.Types where

import Prelude

import Shared.User (ProfileVisibility)

type SM =
      ( email ∷ String
      , emailConfirmation ∷ String
      , password ∷ String
      , erroredFields ∷ Array String
      , passwordConfirmation ∷ String
      , profileVisibility :: ProfileVisibility
      , hideSuccessMessage :: Boolean
      , confirmTermination ∷ Boolean
      )

type SettingsModel = Record SM

data SettingsMessage
      = SetSField (SettingsModel → SettingsModel)
      | ChangeEmail
      | ChangePassword
      | ToggleTerminateAccount
      | ChangeVisibility
      | TerminateAccount --very bad

data ProfileVisibilityId = ProfileVisibilityId

instance Show ProfileVisibilityId where
      show _ = "profile-visibility"