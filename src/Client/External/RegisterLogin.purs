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
import Data.String.Pattern(Pattern(..))
import Data.String.Common as CC

data Endpoint = Register | Login

derive instance eqEndpoint :: Eq Endpoint

foreign import grecaptchaExecute :: Effect Unit
foreign import grecaptchaReset :: Effect Unit

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
			let data' = RegisterLogin {
				email: email,
				password: password,
				captchaResponse: captchaResponse
			}
			C.post url data' enter (const (liftEffect grecaptchaReset))

	where   url = ?hole
		-- the location to go after login is either the query parameter next or /im
		next ["?next=", destination] = destination
		next _ = "/im"

		enter (Token { tokenGET : tokenGET, tokenPOST : tokenPOST }) = liftEffect $ do
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
				splitQueryString <- CC.split (Pattern "=") <$> C.search
				C.setLocation $ next splitQueryString