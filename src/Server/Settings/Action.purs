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
import Shared.Unsafe as SU

blankEmailMessage :: String
blankEmailMessage = "Email and confirmation are required"

blankPasswordMessage :: String
blankPasswordMessage = "Password and confirmation are required"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

changeEmail :: PrimaryKey -> String -> ServerEffect Ok
changeEmail userID email = do
      when (DS.null email) $ SR.throwBadRequest blankEmailMessage
      maybeUser <- SDU.userBy $ Email email
      when (DM.isJust maybeUser) $ SR.throwBadRequest emailAlreadyRegisteredMessage
      SSD.changeEmail userID email
      pure ok

changePassword :: PrimaryKey -> String -> ServerEffect Ok
changePassword userID password = do
      when (DS.null password) $ SR.throwBadRequest blankPasswordMessage
      hash <- ST.hashPassword password
      SSD.changePassword userID hash
      pure ok

terminateAccount :: PrimaryKey -> ServerEffect Ok
terminateAccount userID = do
      SSD.terminateAccount userID
      pure ok

