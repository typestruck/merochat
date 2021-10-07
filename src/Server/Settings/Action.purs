module Server.Settings.Action where

import Prelude
import Server.Types
import Shared.ContentType

import Server.AccountValidation as SA
import Server.Settings.Database as SSD
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

changeVisibility ∷ Int → ProfileVisibility -> ServerEffect Unit
changeVisibility loggedUserID pv = SSD.changeVisibility loggedUserID pv