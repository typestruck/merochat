module Server.Settings.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe as DM
import Data.String as DS
import Server.Database.User as SDU
import Server.Ok (ok)
import Server.Response as SR
import Server.Settings.Database as SSD
import Server.Token as ST

blankEmailMessage :: String
blankEmailMessage = "Email and confirmation are required"

blankPasswordMessage :: String
blankPasswordMessage = "Password and confirmation are required"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

changeEmail :: PrimaryKey -> String -> ServerEffect Ok
changeEmail loggedUserID email = do
      when (DS.null email) $ SR.throwBadRequest blankEmailMessage
      maybeUser <- SDU.userBy $ Email email
      when (DM.isJust maybeUser) $ SR.throwBadRequest emailAlreadyRegisteredMessage
      SSD.changeEmail loggedUserID email
      pure ok

changePassword :: PrimaryKey -> String -> ServerEffect Ok
changePassword loggedUserID password = do
      when (DS.null password) $ SR.throwBadRequest blankPasswordMessage
      hash <- ST.hashPassword password
      SSD.changePassword loggedUserID hash
      pure ok

terminateAccount :: PrimaryKey -> ServerEffect Ok
terminateAccount loggedUserID = do
      SSD.terminateAccount loggedUserID
      pure ok

