--missing tests
module Server.Settings.Action where

import Prelude
import Server.Types
import Shared.Types

import Server.AccountValidation as SA
import Server.Settings.Database as SSD

changeEmail :: PrimaryKey -> String -> ServerEffect Unit
changeEmail loggedUserID rawEmail = do
      email <- SA.validateEmail rawEmail
      SA.validateExistingEmail email
      SSD.changeEmail loggedUserID email

changePassword :: PrimaryKey -> String -> ServerEffect Unit
changePassword loggedUserID password = do
      hash <- SA.validatePassword password
      SSD.changePassword loggedUserID hash

terminateAccount :: PrimaryKey -> ServerEffect Unit
terminateAccount loggedUserID = SSD.terminateAccount loggedUserID