module Server.Landing.Action where

import Prelude
import Shared.Types

import Data.String as S
import HTTPure (ResponseM)
import HTTPure as H
import Server.Response as R
import Server.Types
import Server.Database.User as SDA
import Run.Except as RE
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Generic.Rep (class Generic)

register :: String -> RegisterLogin -> ServerEffect Token
register remoteIP (RegisterLogin registerLogin) = do
	if S.null registerLogin.email || S.null registerLogin.password then
		RE.throw $ BadRequest {reason :  "Invalid email or password" }
	 else do
		user <- SDA.userBy $ Email registerLogin.email
		pure $ Token {tokenGET: "", tokenPOST: ""}
		-- user ifNotNil: [ BadRequest signal: 'Email already registered' ].
		-- MelanchatConfiguration development
		-- 	ifFalse: [ ((NeoJSONReader
		-- 			fromString:
		-- 				(ZnClient new
		-- 					url: 'https://www.google.com/recaptcha/api/siteverify';
		-- 					formAt: #secret put: MelanchatConfiguration captchaSecret;
		-- 					formAt: #response put: captchaResponse;
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
