module Client.IM.WebSocket (sendPayload, module WSW, module WSEM, module WSEE) where

import Prelude
import Shared.IM.Types

import Client.Common.Storage as CCS
import Effect (Effect)
import Shared.JSON as SJ
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.Event.EventTypes as WSEE
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket hiding(sendString) as WSW
import Web.Socket.WebSocket as WSWS

sendPayload :: WebSocket -> WebSocketPayloadServer -> Effect Unit
sendPayload ws payload = do
        token <- CCS.getToken
        WSWS.sendString ws <<< SJ.toJSON $ WebSocketTokenPayloadServer token payload