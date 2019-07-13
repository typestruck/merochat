module Server.Landing.Action where

import Prelude
import Shared.Types

import Data.String as DS
import HTTPure (ResponseM)
import HTTPure as H
import Data.Maybe as DM
import Server.Response as SRR
import Server.Types
import Server.Database.User as SDA
import Run.Except as RE
import Node.HTTP.Client as NTC
import Run.Reader as RR
import Run as R
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Effect.Console as EC
import Data.Either(Either(..))
import Data.Tuple(Tuple(..))
import Data.Maybe(Maybe(..))
import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.FormURLEncoded as DF

register :: String -> RegisterLogin -> ServerEffect Token
register remoteIP (RegisterLogin registerLogin) = do
	when (DS.null registerLogin.email || DS.null registerLogin.password) <<< RE.throw $ BadRequest { reason: "Invalid email or password" }

	user <- SDA.userBy $ Email registerLogin.email

	when (DM.isJust user) <<< RE.throw $ BadRequest { reason:  "Email already registered" }

	{ configuration : Configuration configuration } <- RR.ask
	response <- A.request $ A.defaultRequest {
			url = "https://www.google.com/recaptcha/api/siteverify",
			method = Left POST,
			responseFormat = RF.string,
			content = Just <<< RB.formURLEncoded $ DF.fromArray [
				Tuple "secret" $ Just configuration.captchaSecret,
				Tuple "response" registerLogin.captchaResponse
			]
		}
	case response.body of
		Right payload ->
			if response.status == StatusCode 200 then
				R.liftEffect $ EC.log payload
			 else
				R.liftEffect $ EC.log response.statusText
		Left left -> RE.throw $ InternalError { reason: left }

	pure $ Token {tokenGET: "", tokenPOST: ""}
	-- MelanchatConfiguration development
	-- 	ifFalse: [ ((NeoJSONReader
	-- 			fromString:
	-- 				(ZnClient new
	-- 					formAt: #remoteip put: remoteIP;
	-- 					post)) at: #success)
	-- 			ifFalse: [ BadRequest signal: 'Incorrect captcha' ] ].
	-- ^ Melanchat
	-- 	createToken:
	-- 		((UsersDB new
	-- 					name: self generateName;
	-- 					password: (self class hashPassword: password);
	-- 					email: email;
	-- 					headline: self generateHeadline;
	-- 					description: self generateDescription;
	-- 					insert))
