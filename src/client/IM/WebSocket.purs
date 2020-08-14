module Client.IM.WebSocket where

import Prelude
import Shared.IM.Types

import Client.Common.Storage as CCS
import Effect (Effect)
import Shared.JSON as SJ
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

sendPayload :: WebSocket -> WebSocketPayloadServer -> Effect Unit
sendPayload ws payload = do
        token <- CCS.getToken
        WSW.sendString ws <<< SJ.toJSON $ WebSocketTokenPayloadServer token payload