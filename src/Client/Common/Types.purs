module Client.Common.Types where

import Effect (Effect)
import Prelude
import Web.Socket.WebSocket (WebSocket)

type WebSocketHandler = {
        sendString :: WebSocket -> String -> Effect Unit
}

data ReceivedUser a = New a | Existing a