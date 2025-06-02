module Client.Im.Pwa where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NoMessages)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Data.Traversable as DT
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Shared.Im.Types (ImModel)
import Shared.Options.Topic as SOT
import Web.HTML (Navigator)
import Web.HTML as WH
import Web.HTML.Window as WHW

foreign import register_ ∷ EffectFn2 Navigator String Unit

foreign import data Registration ∷ Type

foreign import data Subscription ∷ Type

foreign import ready_ ∷ EffectFn2 Navigator (Registration → Effect Unit) Unit

foreign import getSubscription_ ∷ EffectFn2 Registration (Nullable Subscription → Effect Unit) Unit

foreign import subscribe_ ∷ EffectFn2 Registration (Subscription → Effect Unit) Unit

foreign import topicBody_ ∷ EffectFn2 Subscription String String

subscribe ∷ Registration → (Subscription → Effect Unit) → Effect Unit
subscribe = EU.runEffectFn2 subscribe_

topicBody ∷ Subscription → String → Effect String
topicBody = EU.runEffectFn2 topicBody_

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

startPwa ∷ ImModel -> NoMessages
startPwa model = model /\ [ startIt ]
      where startIt = EC.liftEffect do
                  registerServiceWorker model.user.id
                  pure Nothing

registerServiceWorker ∷ Int → Effect Unit
registerServiceWorker id = do
      window ← WH.window
      navigator ← WHW.navigator window
      register navigator "/sw.js"
      ready navigator (subscribePush id)

-- | Subscribe to push notifications
subscribePush ∷ Int → Registration → Effect Unit
subscribePush id registration = getSubscription registration handler
      where
      handler existing = case DN.toMaybe existing of
            Nothing → subscribe registration topic
            Just s → topic s -- double subscription?

      topic subscription = do
            body ← topicBody subscription $ spy "toppic" (SOT.makeTopic id)
            EA.launchAff_ <<< void <<< CCN.silentResponse $ request.topic
                  { params: { path: Nil }
                  , body
                  }