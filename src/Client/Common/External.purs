module Client.Common.External where

import Prelude

import Browser.Cookie as BC
import Browser.Cookies.Data (CookieOpts(..), SetCookie(..), Cookie(..))
import Client.Common (tokenKey)
import Client.Common as CC
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Shared.Types (RegisterLogin(..), Token(..))
import Shared.Cookies (cookieName)

-- | Abstracts the validation common to register and login
validateEmailPassword :: Effect (Maybe RegisterLogin)
validateEmailPassword = do
        emailElement <- CC.querySelector "#email"
        passwordElement <- CC.querySelector "#password"
        email <- CC.value emailElement
        password <- CC.value passwordElement

        if DS.null email || DS.null password then do
                CC.alert "Email and password are mandatory"
                pure Nothing
         else pure <<< Just $ RegisterLogin {
                                email: email,
                                password: password,
                                captchaResponse: Nothing
                        }

login :: Token -> String -> Effect Unit
login (Token { tokenGET, tokenPOST }) redirect =  do
        BC.setCookie $ SetCookie {
                cookie : Cookie {
                        key : cookieName,
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
        CC.setItem tokenKey tokenPOST
        CC.setLocation redirect