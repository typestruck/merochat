module Server.Settings.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Server.Database.User as SDU
import Server.Ok (ok)
import Server.Response as SR
import Server.Settings.Database as SSD
import Server.Token as ST
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)

invalidEmailMessage :: String
invalidEmailMessage = "Valid email is required"

invalidPasswordMessage :: String
invalidPasswordMessage = "Password must be between " <> show passwordMinCharacters <> " and " <> show  passwordMaxCharacters

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

changeEmail :: PrimaryKey -> String -> ServerEffect Ok
changeEmail loggedUserID rawEmail = do
      let email = DS.trim rawEmail
      when (DS.length email > emailMaxCharacters || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email)) $ SR.throwBadRequest invalidEmailMessage
      maybeUser <- SDU.userBy $ Email email
      when (DM.isJust maybeUser) $ SR.throwBadRequest emailAlreadyRegisteredMessage
      SSD.changeEmail loggedUserID email
      pure ok

changePassword :: PrimaryKey -> String -> ServerEffect Unit
changePassword loggedUserID rawPassword = do
      let password = DS.trim rawPassword
      when (DS.length password < passwordMinCharacters || DS.length password > passwordMaxCharacters) $ SR.throwBadRequest invalidPasswordMessage
      hash <- ST.hashPassword password
      SSD.changePassword loggedUserID hash

terminateAccount :: PrimaryKey -> ServerEffect Unit
terminateAccount loggedUserID = SSD.terminateAccount loggedUserID