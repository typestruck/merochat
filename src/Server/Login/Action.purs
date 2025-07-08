module Server.Login.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Server.AccountValidation as SA
import Server.Database.Users (By(..))
import Server.Database.Users as SDU
import Server.Response as SR
import Server.Token as ST
import Shared.Account (EmailPassword)

invalidLogin ∷ String
invalidLogin = "Email not registered or incorrect password"

login ∷ EmailPassword → ServerEffect String
login ep = do
      email ← SA.validateEmail ep.email
      hash ← SA.validatePassword ep.password
      maybeUser ← SDU.userBy $ Email email
      case maybeUser of
            Nothing → SR.throwBadRequest invalidLogin
            Just user → do
                  when (Just hash /= user.password) $ SR.throwBadRequest invalidLogin
                  ST.createToken user.id