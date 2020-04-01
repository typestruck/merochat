module Client.Common.Types where

import Effect (Effect)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Generic.Rep (class Generic)
import Prelude
import Web.Socket.WebSocket (WebSocket)

--REFACTOR: these types are basically IM only

type WebSocketHandler = {
        sendPayload :: forall v value. Generic value v => EncodeRep v => WebSocket -> value -> Effect Unit
}

data ReceivedUser a = New a | Existing a