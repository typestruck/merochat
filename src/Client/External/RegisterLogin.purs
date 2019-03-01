module RegisterLogin where

import Prelude

import Common as C
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Type.Data.Boolean (kind Boolean)
import Data.String as S
import Data.Maybe(Maybe(..))
import Effect.Aff as A
import Shared.Types
import Common(tokenKey)
import Data.Unit(unit)
import Browser.Cookie as BC
import Browser.Cookies.Data(CookieOpts(..), SetCookie(..), Cookie(..))

data Endpoint = Register | Login

derive instance eqEndpoint :: Eq Endpoint

foreign import grecaptchaExecute :: Effect Unit

registerLogin :: Endpoint -> Boolean -> Maybe String -> Effect Unit
registerLogin endpoint captcha captchaResponse = do
	emailElement <- C.querySelector "#email"
	passwordElement <- C.querySelector "#password"
	email <- C.value emailElement
	password <- C.value passwordElement

	if S.null email || S.null password then do
		C.alert "Email and password are mandatory"
		pure unit
	 else if captcha then
		grecaptchaExecute
	 else
		A.launchAff_ $ do
			Token { tokenGET : tokenGET, tokenPOST : tokenPOST } <- C.post url $ RegisterLogin {
				email: email,
				password: password,
				captchaResponse: captchaResponse
			}
			liftEffect $ do
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

	where url | endpoint == Register = "/register"
		  | otherwise = "/login"

	        --     location.href = (new URLSearchParams(document.location.search.substring(1))).get('next') || '/im'
	        -- }, error => {
	        --     if (window['grecaptcha'])
	        --         grecaptcha.reset();
	        --     C.alert $ error.errorMessage
	        -- })