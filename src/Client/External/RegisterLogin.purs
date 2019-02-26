module RegisterLogin where

import Prelude

import Common as C
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (kind Boolean)
import Data.String as S

data Endpoint = Register | Login

foreign import grecaptchaExecute :: Effect Unit

registerOrLogin :: Endpoint -> Boolean -> String -> Effect Unit
registerOrLogin endpoint captcha captchaResponse = do
	emailElement <- C.querySelector "#email",
        passwordElement <-  C.querySelector "#password"
	email <- C.value emailElement
	password <- C.value passwordElement

    	if S.null email || S.null password then
        	C.alert "Email and password are mandatory"
        	pure ()
    	else if captcha then
        	grecaptchaExecute
	else
	        post(url, {
	            email: email,
	            password: password,
	            captchaResponse: captchaResponse
	        }, token => {
	            //tokenPOST is a mitigation for csrf/cookie interception (since zinc http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application
	            document.cookie = `melanchat=${token.tokenGET};max-age=3471300000;path=/`
	            localStorage.setItem('token', token.tokenPOST)
	            location.href = (new URLSearchParams(document.location.search.substring(1))).get('next') || '/im'
	        }, error => {
	            if (window['grecaptcha'])
	                grecaptcha.reset();
	            C.alert $ error.errorMessage
	        })