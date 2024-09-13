module Client.Im.WebSocket where

import Prelude

import Client.Common.Location as CCD
import Effect (Effect)
import Environment (production)
import Shared.Im.Types (WebSocketPayloadServer)
import Shared.Json as SJ
import Shared.Options.WebSocket (externalPort, localPort)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSWS

createWebSocket ∷ Effect WebSocket
createWebSocket = do
      hostName ← CCD.hostName
      WSWS.create (protocol <> hostName <> endpoint) []
      where
      protocol
            | production = "wss://"
            | otherwise = "ws://"
      endpoint
            | production = "/ws:" <> show externalPort
            | otherwise = ":" <> show localPort

sendPayload ∷ WebSocket → WebSocketPayloadServer → Effect Unit
sendPayload ws = WSWS.sendString ws <<< SJ.toJson
