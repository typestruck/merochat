module Client.Im.Pwa where

import Prelude

import Client.AppId (imAppId)
import Client.Dom as CCD
import Client.Im.Flame (NoMessages)
import Client.Im.History as CIH
import Client.Network (routes)
import Client.Network as CCN
import Data.Array as DA
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Data.Traversable as DT
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as EU
import Environment (vapidPublicKey)
import Flame.Subscription as FS
import Foreign (Foreign)
import Foreign as F
import Shared.Im.Types (ClientMessagePayload, ImMessage(..), MessageStatus(..), WebSocketPayloadClient(..), ImModel)
import Shared.Unsafe as SU
import Web.HTML (Navigator)
import Web.HTML as WH
import Web.HTML.Window as WHW

data SwMessage = OpenChat Int | NotChatting

foreign import register_ ∷ EffectFn2 Navigator String Unit

foreign import data Registration ∷ Type

foreign import data Subscription ∷ Type

foreign import ready_ ∷ EffectFn2 Navigator (Registration → Effect Unit) Unit

foreign import getSubscription_ ∷ EffectFn2 Registration (Nullable Subscription → Effect Unit) Unit

foreign import subscribe_ ∷ EffectFn3 String Registration (Subscription → Effect Unit) Unit

foreign import receiveMessage_ ∷ EffectFn1 (String → Foreign → Effect Unit) Unit

foreign import postMessage_ ∷ EffectFn2 String Foreign Unit

foreign import serializeSubscription ∷ Subscription → String

subscribe ∷ Registration → (Subscription → Effect Unit) → Effect Unit
subscribe = EU.runEffectFn3 subscribe_ vapidPublicKey

getSubscription ∷ Registration → (Nullable Subscription → Effect Unit) → Effect Unit
getSubscription = EU.runEffectFn2 getSubscription_

ready ∷ Navigator → (Registration → Effect Unit) → Effect Unit
ready = EU.runEffectFn2 ready_

register ∷ Navigator → String → Effect Unit
register = EU.runEffectFn2 register_

receiveMessage ∷ (String → Foreign → Effect Unit) → Effect Unit
receiveMessage = EU.runEffectFn1 receiveMessage_

postMessage ∷ SwMessage → Effect Unit
postMessage message = do
      case message of
            OpenChat userId → EU.runEffectFn2 postMessage_ "read" $ F.unsafeToForeign { userId }
            NotChatting → EU.runEffectFn2 postMessage_ "not-chatting" $ F.unsafeToForeign DN.null

-- | Check if merochat is running as a progressive web application
checkPwa ∷ Effect Boolean
checkPwa = do
      matches ← DT.traverse CCD.mediaMatches [ "fullscreen", "standalone", "minimal-ui" ]
      pure $ DT.or matches

startPwa ∷ ImModel → NoMessages
startPwa model = model /\ [ startIt ]
      where
      startIt = EC.liftEffect do
            registerServiceWorker
            pure Nothing

registerServiceWorker ∷ Effect Unit
registerServiceWorker = do
      window ← WH.window
      navigator ← WHW.navigator window
      register navigator "/sw.js"
      receiveMessage handler
      where
      handler message payload
            | message == "resume" = FS.send imAppId <<< ResumeChat $ F.unsafeFromForeign payload
            | message == "pushed" = FS.send imAppId <<< PushedMessages $ F.unsafeFromForeign payload
            | otherwise = pure unit

--when messages are received by the service, we update the chat history for a snappier feel
-- but these message might already be in there
receiveMessageFromPush ∷ Array ClientMessagePayload → ImModel → NoMessages
receiveMessageFromPush payload model = model { contacts = map update model.contacts } /\ []
      where
      firstMessage = SU.fromJust $ DA.head payload
      makeHistory message =
            { status: Received
            , sender: message.senderId
            , recipient: message.recipientId
            , id: message.id
            , edited: false
            , reaction: Nothing
            , content: message.content
            , date: message.date
            }
      update contact
            | contact.user.id == firstMessage.senderId = contact
                    { history = if _.id <$> DA.last contact.history < Just firstMessage.id then CIH.fixHistory (contact.history <> map makeHistory payload) else contact.history
                    }
            | otherwise = contact

updateSubscription ∷ ImModel → NoMessages
updateSubscription model = model /\ [ update ]
      where
      update = EC.liftEffect do
            window ← WH.window
            navigator ← WHW.navigator window
            ready navigator start
            pure Nothing

      start registration = getSubscription registration (handler registration)

      handler registration existing = case DN.toMaybe existing of
            Nothing → subscribe registration (EA.launchAff_ <<< sendSubscription)
            Just s → EA.launchAff_ $ sendSubscription s

      sendSubscription subscription = void <<< CCN.silentRequest $ routes.im.subscribe { body: { token: serializeSubscription subscription } }

