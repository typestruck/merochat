module Client.Im.WebSocket (sendPayload, closeWith, module WSW, module WSEM, module WSEE, createWebSocket) where

import Prelude
import Shared.Im.Types

import Client.Common.Location as CCD
import Effect (Effect)
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Environment (production)
import Shared.Json as SJ
import Shared.Options.WebSocket (port)
import Web.Socket.Event.EventTypes as WSEE
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSWS
import Web.Socket.WebSocket hiding (sendString, create) as WSW

foreign import closeWith_ ∷ EffectFn3 WebSocket Int String Unit

createWebSocket ∷ Effect WebSocket
createWebSocket = do
      hostName ← CCD.hostName
      WSWS.create (protocol <> hostName <> endpoint) []
      where
      protocol
            | production = "wss://"
            | otherwise = "ws://"
      endpoint
            | production = "/ws"
            | otherwise = ":" <> show port

sendPayload ∷ WebSocket → WebSocketPayloadServer → Effect Unit
sendPayload ws = WSWS.sendString ws <<< SJ.toJson

closeWith ∷ WebSocket → Int → String → Effect Unit
closeWith = EU.runEffectFn3 closeWith_