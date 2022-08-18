module Client.Common.Types where

import Prelude

import Web.Socket.WebSocket (WebSocket)

type CurrentWebSocket =
      { ponged ∷ Boolean
      , webSocket ∷ WebSocket
      }