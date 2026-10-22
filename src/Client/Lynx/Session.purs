module Client.Lynx.Session where

import Prelude

import Effect (Effect)

foreign import initialize ∷ Effect Unit

foreign import clear ∷ Effect Unit

foreign import isAuthenticated ∷ Effect Boolean

foreign import onSessionEnded ∷ Effect Unit → Effect Unit

foreign import isNativeSession ∷ Effect Boolean
