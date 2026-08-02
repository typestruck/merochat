module Client.Im.WebSocket where

import Prelude

import Client.Im.WebSocket.Connection (WebSocket)
import Client.Im.WebSocket.Connection as WSWS
import Client.Location as CCD
import Effect (Effect)
import Environment (production)
import Shared.Im.Types (WebSocketPayloadServer)
import Shared.Json as SJ
import Shared.Options.WebSocket (externalPort, localPort)
import Unsafe.Coerce as EU

createWebSocket ∷ Effect WebSocket
createWebSocket = do
      --hostName ← CCD.hostName
      WSWS.create "wss://mero.chat" [] -- (protocol <> hostName <> endpoint) []

sendPayload ∷ WebSocket → WebSocketPayloadServer → Effect Unit
sendPayload ws = WSWS.sendString ws <<< SJ.toJson
