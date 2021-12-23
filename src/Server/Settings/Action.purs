module Server.Settings.Action where

import Prelude
import Server.Types
import Shared.ContentType

import Server.AccountValidation as SA
import Server.Settings.Database as SSD
import Shared.Settings.Types (PrivacySettings)
import Shared.User (ProfileVisibility)

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserID rawEmail = do
      email ← SA.validateEmail rawEmail
      SA.validateExistingEmail email
      SSD.changeEmail loggedUserID email

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserID password = do
      hash ← SA.validatePassword password
      SSD.changePassword loggedUserID hash

terminateAccount ∷ Int → ServerEffect Unit
terminateAccount loggedUserID = SSD.terminateAccount loggedUserID

changePrivacySettings ∷ Int → PrivacySettings -> ServerEffect Unit
changePrivacySettings loggedUserID ps = SSD.changePrivacySettings loggedUserID ps