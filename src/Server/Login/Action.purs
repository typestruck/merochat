module Server.Login.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.String as DS
import Run.Except as RE
import Server.Database.User as SDU
import Server.Token as ST

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

invalidLogin :: String
invalidLogin = "Email not registered or incorrect password"

login :: RegisterLogin -> ServerEffect Token
login (RegisterLogin registerLogin) = do
	when (DS.null registerLogin.email || DS.null registerLogin.password) <<< RE.throw $ BadRequest { reason: invalidUserEmailMessage }

	maybeUser <- SDU.userBy $ Email registerLogin.email
	case maybeUser of
		Nothing -> RE.throw $ BadRequest { reason: invalidLogin }
		Just (User user) -> do
			hashed <- ST.hashPassword registerLogin.password

			when (hashed /= user.password) <<< RE.throw $ BadRequest { reason: invalidLogin }

			ST.createToken user.id