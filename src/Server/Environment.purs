module Server.Environment
      ( adminSecret
      , captchaSecret
      , databaseHost
      , port
      , salt
      , tokenSecret
      , websocketPort
      ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as DN

foreign import port ∷ Int
foreign import websocketPort ∷ Int
foreign import databaseHost_ ∷ Nullable String
foreign import captchaSecret ∷ String
foreign import tokenSecret ∷ String
foreign import salt ∷ String
foreign import adminSecret ∷ String

databaseHost ∷ Maybe String
databaseHost = DN.toMaybe databaseHost_
