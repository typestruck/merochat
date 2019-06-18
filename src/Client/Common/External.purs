module Client.Common.External where

import Prelude

import Client.Common as C
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect)
import Shared.Types (RegisterLogin(..), Token(..))
import Type.Data.Boolean (kind Boolean)
import Browser.Cookie as BC
import Browser.Cookies.Data(CookieOpts(..), SetCookie(..), Cookie(..))
import Client.Common(tokenKey)

-- | Abstracts the validation common to register and login
validateEmailPassword :: Effect (Maybe RegisterLogin)
validateEmailPassword = do
	emailElement <- C.querySelector "#email"
	passwordElement <- C.querySelector "#password"
	email <- C.value emailElement
	password <- C.value passwordElement

	if S.null email || S.null password then do
		C.alert "Email and password are mandatory"
		pure Nothing
	 else pure <<< Just $ RegisterLogin {
				email: email,
				password: password,
				captchaResponse: Nothing
			}

login :: Token -> String -> Effect Unit
login (Token { tokenGET : tokenGET, tokenPOST : tokenPOST }) redirect =  do
	BC.setCookie $ SetCookie {
		cookie : Cookie {
			key : "melanchat",
			value : tokenGET
		},
		opts : Just $ CookieOpts {
			maxAge : Just 3471300000.0,
			expires : Nothing,
			secure : false,
			httpOnly : false,
			samesite : Nothing,
			domain : Nothing,
			path : Just "/"
		}
	}
	C.setItem tokenKey tokenGET
	C.setLocation redirect