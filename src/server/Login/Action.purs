module Server.Login.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.String as DS
import Server.Database.User as SDU
import Server.Token as ST
import Server.Response as SRR

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

invalidLogin :: String
invalidLogin = "Email not registered or incorrect password"

login :: RegisterLogin -> ServerEffect String
login registerLogin = do
        when (DS.null registerLogin.email || DS.null registerLogin.password) $ SRR.throwBadRequest invalidUserEmailMessage
        maybeUser <- SDU.userBy $ Email registerLogin.email
        case maybeUser of
                Nothing -> SRR.throwBadRequest invalidLogin
                Just (RegisterLoginUser user) -> do
                        hashed <- ST.hashPassword registerLogin.password
                        when (hashed /= user.password) $ SRR.throwBadRequest invalidLogin
                        ST.createToken user.id