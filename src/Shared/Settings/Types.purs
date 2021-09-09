module Shared.Settings.Types where

type SM =
      ( email ∷ String
      , emailConfirmation ∷ String
      , password ∷ String
      , erroredFields ∷ Array String
      , passwordConfirmation ∷ String
      , confirmTermination ∷ Boolean
      )

type SettingsModel = Record SM

data SettingsMessage
      = SetSField (SettingsModel → SettingsModel)
      | ChangeEmail
      | ChangePassword
      | ToggleTerminateAccount
      | TerminateAccount --very bad