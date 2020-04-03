module Shared.Cookies where

import Browser.Cookie as BC
import Effect (Effect)
import Prelude

cookieName :: String
cookieName = "melanchat"

removeCookie :: String -> Effect Unit
removeCookie = BC.removeCookie
