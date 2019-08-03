module Server.Token where

import Prelude
import Server.Types
import Shared.Types

import Data.Either as DE
import Data.Int53 (Int53)
import Data.Int53 as DI
import Node.Crypto.Hash as NCHA
import Node.Crypto.Hmac as NCH
import Node.Simple.Jwt (Jwt(..))
import Node.Simple.Jwt as NSJ
import Run as R
import Run.Reader as RR

hashPassword :: String -> ServerEffect String
hashPassword password = do
	{ configuration : Configuration configuration } <- RR.ask
	R.liftEffect $ NCH.hex NCHA.SHA512 configuration.salt password

-- add tests after login is done

createToken :: Int53 -> ServerEffect Token
createToken id = do
	{ configuration : Configuration configuration } <- RR.ask

	Jwt tokenGET <- R.liftEffect <<< NSJ.encode configuration.tokenSecretGET NSJ.HS512 $ show id
	Jwt tokenPOST <- R.liftEffect <<< NSJ.encode configuration.tokenSecretPOST NSJ.HS512 $ show id
	pure $ Token { tokenGET, tokenPOST }

userFromToken :: String -> String -> _ (Maybe Int53)
userFromToken secret = R.liftEffect <<< DE.either (const Nothing) (Just <<< DI.fromInt) <<< NSJ.decode secret <<< Jwt