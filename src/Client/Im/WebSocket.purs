module Client.Im.WebSocket (sendPayload, module WSW, module WSEM, module WSEE, createWebSocket) where

import Prelude
import Shared.Im.Types

import Client.Common.Location as CCD
import Effect (Effect)
import Environment (production)
import Shared.Json as SJ
import Shared.Options.WebSocket (port)
import Web.Socket.Event.EventTypes as WSEE
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSWS
import Web.Socket.WebSocket hiding (sendString, create) as WSW

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