module Client.Im.WebSocket.Events (startWebSocket) where

import Prelude

import Client.Common.Dom as CCD
import Client.Im.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer (IntervalId, TimeoutId)
import Effect.Timer as ET
import Flame.Subscription as FS
import Foreign as FO
import Shared.Im.Types (FullWebSocketPayloadClient(..), ImMessage(..), WebSocketPayloadServer(..))
import Shared.Json as SJ
import Shared.Options.MountPoint (imId)
import Shared.Unsafe as SU
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.Socket.Event.EventTypes (onMessage, onOpen, onClose)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

-- | Reconection, ping and privilege update are done with setInterval
type WebSocketState = {
      webSocket ∷ WebSocket,
      reconnectId:: Maybe TimeoutId,
      pingId:: Maybe IntervalId,
      privilegesId:: Maybe IntervalId
}

-- | Web socket state is kept in a ref since the connection can be closed and recreated by events any time
startWebSocket ∷ Effect (Ref WebSocketState)
startWebSocket = do
      webSocket ← CIW.createWebSocket
      webSocketStateRef ← ER.new { webSocket, reconnectId: Nothing, pingId: Nothing, privilegesId: Nothing  }

      setUpWebsocket webSocketStateRef
      pure webSocketStateRef

-- | Set listeners for web socket events
setUpWebsocket ∷ Ref WebSocketState → Effect Unit
setUpWebsocket webSocketStateRef = do
      state <- ER.read webSocketStateRef

      let webSocketTarget = WSW.toEventTarget state.webSocket
      openListener ← WET.eventListener (handleOpen webSocketStateRef)
      messageListener ← WET.eventListener handleMessage
      closeListener ← WET.eventListener (handleClose webSocketStateRef)

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget

handleOpen :: Ref WebSocketState -> Event -> Effect Unit
handleOpen webSocketStateRef _ = do
      state ← ER.read webSocketStateRef
      --close event may have set up to open a new connection after this timeout
      DM.maybe (pure unit) ET.clearTimeout state.reconnectId
      newPrivilegesId ← ET.setInterval privilegeDelay pollPrivileges
      newPingId ← ET.setInterval pingDelay ping
      ER.modify_ (_ { pingId = Just newPingId, privilegesId = Just newPrivilegesId, reconnectId = Nothing }) webSocketStateRef

      FS.send imId $ ToggleConnected true
      --check if the page needs to be reloaded
      CIW.sendPayload state.webSocket UpdateHash

      where privilegeDelay = 1000 * 60 * 60
            pollPrivileges = FS.send imId PollPrivileges

            pingDelay = 1000 * 30
            ping = do
                  isFocused ← CCD.documentHasFocus
                  FS.send imId $ SendPing isFocused

-- | Handle an incoming (json encoded) message from the server
handleMessage ∷ Event → Effect Unit
handleMessage event = do
      let payload = SU.fromRight <<< CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.fromJust $ WSEM.fromEvent event
      let message = SU.fromRight $ SJ.fromJson payload
      case message of
            Pong p → FS.send imId $ DisplayAvailability p.status --pings are set up when the socket is open
            CloseConnection cc → FS.send imId $ Logout cc --user has been banned or server is on fire
            Content c → do
                  isFocused ← CCD.documentHasFocus
                  FS.send imId $ ReceiveMessage c isFocused --actual site events, like new messages or status updates

-- | Clear intervals and set up new web socket connection after a random timeout
handleClose ∷ Ref WebSocketState → Event → Effect Unit
handleClose webSocketStateRef _ = do
      state ← ER.read webSocketStateRef
      FS.send imId $ ToggleConnected false

      DM.maybe (pure unit) ET.clearInterval state.pingId
      DM.maybe (pure unit) ET.clearInterval state.privilegesId
      ER.modify_ (_ { pingId = Nothing, privilegesId = Nothing }) webSocketStateRef
      --skip if we already are waiting on a timeout
      when (DM.isNothing state.reconnectId) do
            --we need it random so the server is not flooded with a zillion simultaneous connections
            milliseconds ← ERD.randomInt 2000 10000
            id ← ET.setTimeout milliseconds do
                  webSocket <- CIW.createWebSocket
                  ER.modify_ (_ { webSocket = webSocket }) webSocketStateRef
                  setUpWebsocket webSocketStateRef
            ER.modify_ (_ { reconnectId = Just id }) webSocketStateRef