module Client.Common.External where

import Prelude

import Browser.Cookie as BC
import Browser.Cookies.Data (CookieOpts(..), SetCookie(..), Cookie(..))
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Notification as CCN
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Shared.Types (RegisterLogin(..), Token(..))
import Shared.Cookies (cookieName)

-- | Abstracts the validation common to register and login
validateEmailPassword :: Effect (Maybe RegisterLogin)
validateEmailPassword = do
        emailElement <- CCD.querySelector "#email"
        passwordElement <- CCD.querySelector "#password"
        email <- CCD.value emailElement
        password <- CCD.value passwordElement

        if DS.null email || DS.null password then do
                CCN.alert "Email and password are mandatory"
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
        CCS.setItem tokenKey tokenPOST
        CCL.setLocation redirect