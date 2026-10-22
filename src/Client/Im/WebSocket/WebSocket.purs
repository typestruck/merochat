module Client.Im.WebSocket where

import Prelude

import Client.Im.WebSocket.Connection (WebSocket)
import Client.Im.WebSocket.Connection as CIWC
import Effect (Effect)
import Environment (websocketUrl)
import Shared.Im.Types (WebSocketPayloadServer)
import Shared.Json as SJ

createWebSocket ∷ Effect WebSocket
createWebSocket = CIWC.create websocketUrl []

sendPayload ∷ WebSocket → WebSocketPayloadServer → Effect Unit
sendPayload ws = CIWC.sendString ws <<< SJ.toJson
