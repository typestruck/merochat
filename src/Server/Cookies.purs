module Server.Cookies where

import Prelude

import Browser.Cookies.Data (Cookie(..), CookieOpts(..), SameSite(..), SetCookie(..))
import Browser.Cookies.Data as BC
import Data.JSDate as DJ
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Server.Types (ServerEffect)
import Shared.Options.Domain (domain)
import Environment(development)

cookieHeader :: String
cookieHeader = "Set-Cookie"

makeCookieHeader :: String -> ServerEffect (Tuple String String)
makeCookieHeader = pure <<< Tuple cookieHeader <<< BC.encode <<< makeCookie

makeExpiredCookieHeader :: Tuple String String
makeExpiredCookieHeader = Tuple cookieHeader $ BC.encode expiredCookie
      where expiredCookie :: SetCookie
            expiredCookie = DN.over SetCookie expire $ makeCookie ""
            expire cookie = cookie {
                  opts = map update' cookie.opts
            }
            update' :: CookieOpts -> CookieOpts
            update' = DN.over CookieOpts update
            update cookie = cookie {
                  maxAge = Nothing,
                  expires = Just $ DJ.jsdate {
                        day: 1.0,
                        hour: 1.0,
                        millisecond: 1.0,
                        minute: 1.0,
                        month: 1.0,
                        second: 1.0,
                        year: 1900.0
                  }
            }

cookieName :: String
cookieName = "melanchat"

makeCookie :: String -> SetCookie
makeCookie value =
      SetCookie {
            cookie: Cookie { key: cookieName, value },
            opts: Just $ CookieOpts {
                  maxAge: Just 3471300000.0,
                  expires: Nothing,
                  httpOnly: true,
                  samesite: if development then Nothing else Just Strict,
                  domain: if development then Nothing else Just domain,
                  path: Just "/",
                  secure: not development
            }
      }