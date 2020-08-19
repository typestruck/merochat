module Server.Cookies where

import Prelude

import Browser.Cookies.Data (Cookie(..), CookieOpts(..), SameSite(..), SetCookie(..))
import Browser.Cookies.Data as BC
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Run.Reader as RR
import Server.Domain (domain)
import Server.Types (ServerEffect)

makeCookieHeader :: String -> ServerEffect (Tuple String String)
makeCookieHeader token = do
      { configuration: { development } } <- RR.ask
      pure $ Tuple "Set-Cookie" $ makeCookie development token

cookieName :: String
cookieName = "melanchat"

makeCookie :: Boolean -> String -> String
makeCookie isDevelopment value =
      BC.encode $ SetCookie {
            cookie: Cookie { key: cookieName, value },
            opts: Just $ CookieOpts {
                  maxAge: Just 3471300000.0,
                  expires: Nothing,
                  httpOnly: true,
                  samesite: if isDevelopment then Nothing else Just Strict,
                  domain: if isDevelopment then Nothing else Just domain,
                  path: Just "/",
                  secure: not isDevelopment
            }
      }