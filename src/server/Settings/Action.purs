module Server.Settings.Action where

import Prelude
import Shared.Types

import Data.Maybe as DM
import Data.String as DS
import Server.Database.User as SDU
import Server.Response as SSR
import Server.Settings.Database as SSD
import Server.Token as ST
import Server.Types (ServerEffect)
import Shared.Unsafe as SU

blankEmailMessage :: String
blankEmailMessage = "Email and confirmation are required"

emailDoesNotMatchMessage :: String
emailDoesNotMatchMessage = "Email and confirmation do not match"

blankPasswordMessage :: String
blankPasswordMessage = "Password and confirmation are required"

passwordDoesNotMatchMessage :: String
passwordDoesNotMatchMessage = "Password and confirmation do not match"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

changeEmail :: PrimaryKey -> SettingsModel -> ServerEffect Ok
changeEmail userID (SettingsModel { email, emailConfirmation }) = do
        when (DS.null email || DS.null emailConfirmation) $ SSR.throwBadRequest blankEmailMessage
        when (email /= emailConfirmation) $ SSR.throwBadRequest emailDoesNotMatchMessage
        maybeUser <- SDU.userBy $ Email email
        when (DM.isJust maybeUser) $ SSR.throwBadRequest emailAlreadyRegisteredMessage

        SSD.changeEmail userID email
        pure Ok

changePassword :: PrimaryKey -> SettingsModel -> ServerEffect Ok
changePassword userID (SettingsModel { password, passwordConfirmation }) = do
        when (DS.null password || DS.null passwordConfirmation) $ SSR.throwBadRequest blankPasswordMessage
        when (password /= passwordConfirmation) $ SSR.throwBadRequest passwordDoesNotMatchMessage

        hash <- ST.hashPassword password
        SSD.changePassword userID hash
        pure Ok

terminateAccount :: PrimaryKey -> ServerEffect Ok
terminateAccount userID = do
        SSD.terminateAccount userID
        pure Ok

