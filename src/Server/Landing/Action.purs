module Server.Landing.Action where

import Data.String as S
import HTTPure (ResponseM)
import HTTPure as H
import Server.Response as R

register :: String -> RegisterLogin -> ResponseM
register remoteIP registerLogin = do
	if S.null email || S.null password then
		R.badRequest "Invalid email or password"
	else do
		let user = (UsersDB new email: email) selectByEmail.
	user ifNotNil: [ BadRequest signal: 'Email already registered' ].
	MelanchatConfiguration development
		ifFalse: [ ((NeoJSONReader
				fromString:
					(ZnClient new
						url: 'https://www.google.com/recaptcha/api/siteverify';
						formAt: #secret put: MelanchatConfiguration captchaSecret;
						formAt: #response put: captchaResponse;
						formAt: #remoteip put: remoteIP;
						post)) at: #success)
				ifFalse: [ BadRequest signal: 'Incorrect captcha' ] ].
	^ Melanchat
		createToken:
			((UsersDB new
						name: self generateName;
						password: (self class hashPassword: password);
						email: email;
						headline: self generateHeadline;
						description: self generateDescription;
						insert))
