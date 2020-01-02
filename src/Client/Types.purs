module Client.Types where

import Effect (Effect)
import Prelude
import Web.Socket.WebSocket (WebSocket)

type WebSocketHandler = {
        sendString :: WebSocket -> String -> Effect Unit
}