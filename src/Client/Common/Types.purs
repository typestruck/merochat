module Client.Common.Types where

import Prelude

import Web.Socket.WebSocket (WebSocket)

data RequestStatus = Success | Fail

derive instance eqRequestStatus :: Eq RequestStatus

type CurrentWebSocket = {
    ponged :: Boolean,
    webSocket :: WebSocket
}