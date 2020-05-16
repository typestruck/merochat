module Server.Settings.Action where

import Prelude
import Shared.Settings.Types
import Shared.Types

import Data.String as DS
import Server.Response as SSR
import Server.Settings.Database as SSD
import Server.Types (ServerEffect)
import Shared.Settings.Types (SettingsModel(..))
import Shared.Unsafe as SU

blankEmailMessage :: String
blankEmailMessage = "Email and confirmation are required"

emailDoesNotMatchMessage :: String
emailDoesNotMatchMessage = "Email and confirmation do not match"

changeEmail :: PrimaryKey -> SettingsModel -> ServerEffect Ok
changeEmail userID (SettingsModel { email, emailConfirmation }) = do
        when (DS.null email || DS.null emailConfirmation) $ SSR.throwBadRequest blankEmailMessage
        when (email /= emailConfirmation) $ SSR.throwBadRequest emailDoesNotMatchMessage

        SSD.changeEmail userID email
        pure Ok

