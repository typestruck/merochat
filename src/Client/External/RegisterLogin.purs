module RegisterLogin where

import Prelude

import Common as C
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (kind Boolean)
import Data.String as S
import Affjax as AJ
import Effect.Aff as A
import Shared.Types(RegisterLogin(..))
import Affjax.ResponseFormat as RF
import Affjax.RequestBody as RB

data Endpoint = Register | Login

--registerOrLogin :: Endpoint -> Boolean -> String -> Effect Unit
--registerOrLogin endpoint captcha captchaResponse = do
		--emailElement <- C.querySelector "#email"
		--pure ()
	    -- passwordElement <- C.querySelector "#password"
		-- email <- C.value emailElement
		-- password <- C.value passwordElement

    	-- if S.null email || S.null password then
        -- 	C.alert "Email and password are mandatory"
        -- 	pure ()
    	-- else if captcha then
        -- 	grecaptchaExecute
		-- else A.launchAff $ do
		--         response <- AJ.post RF.json url RB.json $ RegisterLogin {
		--             email: email,
		--             password: password,
		--             captchaResponse: captchaResponse
		--         }

		-- 		case response of
		-- 			Left a -> pure ()
		-- 			Left b -> pure ()
			-- token => {
	        --     //tokenPOST is a mitigation for csrf/cookie interception (since zinc http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application
	        --     document.cookie = `melanchat=${token.tokenGET};max-age=3471300000;path=/`
	        --     localStorage.setItem('token', token.tokenPOST)
	        --     location.href = (new URLSearchParams(document.location.search.substring(1))).get('next') || '/im'
	        -- }, error => {
	        --     if (window['grecaptcha'])
	        --         grecaptcha.reset();
	        --     C.alert $ error.errorMessage
	        -- })

foreign import grecaptchaExecute :: Effect Unit