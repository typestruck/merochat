module Server.Landing.Action where

import Prelude
import Server.Types
import Shared.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Either (Either(..))
import Data.Either as DE
import Data.FormURLEncoded as DF
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Int53 (Int53)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect.Console as EC
import HTTPure (ResponseM)
import HTTPure as H
import Node.Crypto.Hash as NCH
import Node.HTTP.Client as NTC
import Node.Simple.Jwt (Jwt(..))
import Node.Simple.Jwt as NSJ
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Bender as SB
import Server.Database.Action as SDA
import Server.Database.User as SDU
import Server.Response as SRR

register :: String -> RegisterLogin -> ServerEffect Token
register remoteIP (RegisterLogin registerLogin) = do
	when (DS.null registerLogin.email || DS.null registerLogin.password) <<< RE.throw $ BadRequest { reason: "Invalid email or password" }

	user <- SDU.userBy $ Email registerLogin.email

	when (DM.isJust user) <<< RE.throw $ BadRequest { reason:  "Email already registered" }

	{ configuration : Configuration configuration } <- RR.ask
	response <- R.liftAff <<< A.request $ A.defaultRequest {
			url = "https://www.google.com/recaptcha/api/siteverify",
			method = Left POST,
			responseFormat = RF.json,
			content = Just <<< RB.formURLEncoded $ DF.fromArray [
				Tuple "secret" $ Just configuration.captchaSecret,
				Tuple "response" registerLogin.captchaResponse
			]
		}
	case response.body of
		Right payload ->
			if response.status == StatusCode 200 then
				DE.either SRR.throwInternalError finish $ DAD.decodeJson payload
			 else
				SRR.throwInternalError response.statusText
		Left left -> SRR.throwInternalError $ RF.printResponseFormatError left
	where   finish (CaptchaResponse {success})
			| success = do
				name <- SB.generateName
				headline <- SB.generateHeadline
				description <- SB.generateDescription
				password <- hashPassword registerLogin.password
				PrimaryKey id <- SDA.createUser {
					email: registerLogin.email,
					name,
					password,
					headline,
					description
				}
				createToken id
			| otherwise = SRR.throwInternalError "Incorrect captcha"
	-- ^ Melanchat
	-- 	createToken:
	-- 		((UsersDB new
	-- 					name: self generateName;
	-- 					password: (self class hashPassword: password);
	-- 					email: email;
	-- 					headline: self generateHeadline;
	-- 					description: self generateDescription;
	-- 					insert))

hashPassword :: String -> ServerEffect String
hashPassword = R.liftEffect <<< NCH.hex NCH.SHA512

createToken :: Int53 -> ServerEffect Token
createToken id = do
	{ configuration : Configuration configuration } <- RR.ask

	Jwt tokenGET <- R.liftEffect <<< NSJ.encode configuration.tokenSecretGET NSJ.HS512 $ show id
	Jwt tokenPOST <- R.liftEffect <<< NSJ.encode configuration.tokenSecretPOST NSJ.HS512 $ show id
	pure $ Token { tokenGET, tokenPOST }