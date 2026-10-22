module Client.Lynx.Session where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Environment (apiUrl, websocketUrl)

foreign import initialize_ ∷ EffectFn2 String String Unit

initialize ∷ Effect Unit
initialize = EU.runEffectFn2 initialize_ apiUrl websocketUrl

foreign import clear ∷ Effect Unit

foreign import isAuthenticated ∷ Effect Boolean

foreign import onSessionEnded ∷ Effect Unit → Effect Unit

foreign import isNativeSession ∷ Effect Boolean
