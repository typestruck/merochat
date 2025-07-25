--REFACTOR: make it its own library

-- | WS node library bindings adapted from https://github.com/FruitieX/purescript-ws
module Server.WebSocket where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as EU
import Node.HTTP (Request)
import Record as R
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons, class Union)

foreign import data WebSocketServer ∷ Type
foreign import data WebSocketConnection ∷ Type
foreign import createWebSocketServer_ ∷ ∀ options. EffectFn2 options (EffectFn1 Unit Unit) WebSocketServer
foreign import onConnection_ ∷ EffectFn2 WebSocketServer (EffectFn2 WebSocketConnection Request Unit) Unit
foreign import onServerError_ ∷ EffectFn2 WebSocketServer (EffectFn1 Error Unit) Unit
foreign import onServerClose_ ∷ EffectFn2 WebSocketServer (EffectFn1 Unit Unit) Unit
foreign import onMessage_ ∷ EffectFn2 WebSocketConnection (EffectFn1 WebSocketMessage Unit) Unit
foreign import onClose_ ∷ EffectFn2 WebSocketConnection (EffectFn2 CloseCode CloseReason Unit) Unit
foreign import onError_ ∷ EffectFn2 WebSocketConnection (EffectFn1 Error Unit) Unit
foreign import sendMessage_ ∷ EffectFn2 WebSocketConnection WebSocketMessage Unit
foreign import close_ ∷ EffectFn3 WebSocketConnection CloseCode CloseReason Unit
foreign import terminate_ ∷ EffectFn1 WebSocketConnection Unit

foreign import lastPing ∷ DateTime → WebSocketConnection → WebSocketConnection
foreign import getLastPing ∷ WebSocketConnection → DateTime

foreign import createWebSocket_ ∷ EffectFn2 String String WebSocketConnection
foreign import onOpen_ ∷ EffectFn2 WebSocketConnection (EffectFn1 Unit Unit) Unit

newtype WebSocketMessage = WebSocketMessage String

derive newtype instance showWSM ∷ Show WebSocketMessage
derive instance newtypeWSM ∷ Newtype WebSocketMessage _

type WebSocketServerOptions = (host ∷ String, backlog ∷ Int)

-- | The port to listen on if calling createWebSocketServerWithPort
newtype Port = Port Int
newtype CloseCode = CloseCode Int
newtype CloseReason = CloseReason String

-- | Creates a WebSocket.Server and internally a HTTP server
-- | which binds to a given port
-- |
-- | The supplied callback is called when the created HTTP server
-- | starts listening.
createWebSocketServerWithPort ∷ ∀ options options' trash. Union options options' WebSocketServerOptions ⇒ Lacks "port" options ⇒ Cons "port" Port options trash ⇒ Port → { | options } → (Unit → Effect Unit) → Effect WebSocketServer
createWebSocketServerWithPort (Port port) options callback = EU.runEffectFn2 createWebSocketServer_ options' callback'
      where
      options' = R.insert (Proxy ∷ Proxy "port") port options
      callback' = EU.mkEffectFn1 callback

-- | Attaches a connection event handler to a WebSocketServer
onConnection ∷ WebSocketServer → (WebSocketConnection → Request → Effect Unit) → Effect Unit
onConnection server callback = EU.runEffectFn2 onConnection_ server (EU.mkEffectFn2 callback)

-- | Attaches an error event handler to a WebSocketServer
onServerError ∷ WebSocketServer → (Error → Effect Unit) → Effect Unit
onServerError server callback = EU.runEffectFn2 onServerError_ server (EU.mkEffectFn1 callback)

onServerClose ∷ WebSocketServer → (Unit → Effect Unit) → Effect Unit
onServerClose server callback = EU.runEffectFn2 onServerClose_ server (EU.mkEffectFn1 callback)

-- | Attaches a message event handler to a WebSocketConnection
onMessage ∷ WebSocketConnection → (WebSocketMessage → Effect Unit) → Effect Unit
onMessage ws callback = EU.runEffectFn2 onMessage_ ws (EU.mkEffectFn1 callback)

-- | Attaches a close event handler to a WebSocketConnection
onClose ∷ WebSocketConnection → (CloseCode → CloseReason → Effect Unit) → Effect Unit
onClose ws callback = EU.runEffectFn2 onClose_ ws (EU.mkEffectFn2 callback)

-- | Attaches an error event handler to a WebSocketConnection
onError ∷ WebSocketConnection → (Error → Effect Unit) → Effect Unit
onError ws callback = EU.runEffectFn2 onError_ ws (EU.mkEffectFn1 callback)

-- | Send a message over a WebSocketConnection
sendMessage ∷ WebSocketConnection → WebSocketMessage → Effect Unit
sendMessage ws message = EU.runEffectFn2 sendMessage_ ws message

-- | Initiate a closing handshake
close ∷ WebSocketConnection → Effect Unit
close ws = EU.runEffectFn3 close_ ws (CloseCode 1000) (CloseReason "Closed by server")

terminate ∷ WebSocketConnection → Effect Unit
terminate ws = EU.runEffectFn1 terminate_ ws

-- | Client
createWebSocket ∷ String → String → Effect WebSocketConnection
createWebSocket = EU.runEffectFn2 createWebSocket_

onOpen ∷ WebSocketConnection → (Unit → Effect Unit) → Effect Unit
onOpen ws callback = EU.runEffectFn2 onOpen_ ws (EU.mkEffectFn1 callback)
