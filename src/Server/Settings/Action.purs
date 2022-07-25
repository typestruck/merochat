module Server.Settings.Action where

import Prelude
import Server.Types

import Server.AccountValidation as SA
import Server.Settings.Database as SSD
import Shared.Settings.Types (PrivacySettings)

settings :: Int -> ServerEffect PrivacySettings
settings loggedUserId = SSD.privacySettings loggedUserId

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserId rawEmail = do
      email ← SA.validateEmail rawEmail
      SA.validateExistingEmail email
      SSD.changeEmail loggedUserId email

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserId password = do
      hash ← SA.validatePassword password
      SSD.changePassword loggedUserId hash

terminateAccount ∷ Int → ServerEffect Unit
terminateAccount loggedUserId = SSD.terminateAccount loggedUserId

changePrivacySettings ∷ Int → PrivacySettings -> ServerEffect Unit
changePrivacySettings loggedUserId ps = SSD.changePrivacySettings loggedUserId ps