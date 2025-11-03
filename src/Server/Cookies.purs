-- we could use payload functions instead of this and remove the dependency on browser cookies data

module Server.Cookies where

import Prelude

import Browser.Cookies.Data (Cookie(..), CookieOpts(..), SetCookie(..))
import Browser.Cookies.Data as BC
import Data.JSDate as DJ
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Environment (production)
import Safe.Coerce as SC
import Server.Effect (ServerEffect)
import Shared.Options.Domain (domain)

cookieHeader ∷ String
cookieHeader = "Set-Cookie"

makeCookieHeader ∷ String → ServerEffect (Tuple String String)
makeCookieHeader = pure <<< Tuple cookieHeader <<< BC.encode <<< makeCookie

makeExpiredCookieHeader ∷ Tuple String String
makeExpiredCookieHeader = Tuple cookieHeader $ BC.encode expiredCookie
      where
      expiredCookie ∷ SetCookie
      expiredCookie = SetCookie <<< expire $ SC.coerce makeCookie ""
      expire cookie = cookie
            { opts = map update' cookie.opts
            }

      update' ∷ CookieOpts → CookieOpts
      update' (CookieOpts opts) = CookieOpts $ update opts
      update cookie = cookie
            { maxAge = Nothing
            , expires = Just $ DJ.jsdate
                    { day: 1.0
                    , hour: 1.0
                    , millisecond: 1.0
                    , minute: 1.0
                    , month: 1.0
                    , second: 1.0
                    , year: 1900.0
                    }
            }

cookieName ∷ String
cookieName = "merochat"

makeCookie ∷ String → SetCookie
makeCookie value =
      SetCookie
            { cookie: Cookie { key: cookieName, value }
            , opts: Just $ CookieOpts
                    { maxAge: Just 3471300000.0
                    , expires: Just $ DJ.jsdate
                            { day: 1.0
                            , hour: 1.0
                            , millisecond: 1.0
                            , minute: 1.0
                            , month: 1.0
                            , second: 1.0
                            , year: 2300.0
                            }
                    , httpOnly: true
                    , samesite: Nothing
                    , domain: if production then Just domain else Nothing
                    , path: Just "/"
                    , secure: production
                    }
            }