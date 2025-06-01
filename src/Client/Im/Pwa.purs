module Client.Im.Pwa where

import Prelude

import Client.Common.Dom as CCD
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Data.Traversable as DT
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Web.HTML (Navigator)
import Web.HTML as WH
import Web.HTML.Window as WHW

foreign import register_ ∷ EffectFn2 Navigator String Unit

foreign import data Registration ∷ Type

foreign import data Subscription ∷ Type

foreign import ready_ ∷ EffectFn2 Navigator (Registration → Effect Unit) Unit

foreign import getSubscription_ ∷ EffectFn2 Registration (Nullable Subscription → Effect Unit) Unit

foreign import subscribe_ ∷ EffectFn1 Registration Unit

subscribe ∷ Registration → Effect Unit
subscribe = EU.runEffectFn1 subscribe_

getSubscription ∷ Registration → (Nullable Subscription → Effect Unit) → Effect Unit
getSubscription = EU.runEffectFn2 getSubscription_

ready ∷ Navigator → (Registration → Effect Unit) → Effect Unit
ready = EU.runEffectFn2 ready_

register ∷ Navigator → String → Effect Unit
register = EU.runEffectFn2 register_

-- | Check if merochat is running as a progressive web application
checkPwa ∷ Effect Boolean
checkPwa = do
      matches ← DT.traverse CCD.mediaMatches [ "fullscreen", "standalone", "minimal-ui" ]
      pure $ DT.or matches

registerServiceWorker ∷ Effect Unit
registerServiceWorker = do
      window ← WH.window
      navigator ← WHW.navigator window
      register navigator "/sw.js"
      ready navigator subscribePush

-- | Subscribe to push notifications
subscribePush ∷ Registration → Effect Unit
subscribePush registration = do
      getSubscription registration handler
      where
      handler existing = case DN.toMaybe existing of
            Nothing → subscribe registration
            Just s → pure unit
