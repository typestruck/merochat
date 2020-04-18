module Client.Common.Cookies where

import Prelude

import Browser.Cookie as BC
import Browser.Cookies.Data (Cookie(..))
import Effect (Effect)
import Shared.Cookies (cookieName)
import Shared.Unsafe as SU

removeMelanchatCookie :: Effect Unit
removeMelanchatCookie = BC.removeCookie cookieName

getMelanchatCookie :: Effect String
getMelanchatCookie = (\(Cookie { value }) -> value) <<< SU.unsafeFromJust "getMelanchatCookie" <$> BC.getCookie cookieName