module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Either as DE
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String as DS
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (Pool)
import Effect (Effect)
import Effect.Aff as CMEC
import Effect.Aff as EA
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Node.HTTP (Request)
import Partial.Unsafe as PU
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.Token as ST
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Server.Wheel as SWL
import Shared.JSON as SJ

handleError :: Error -> Effect Unit
handleError = EC.log <<< show

--here it seems like the only way is get the cookie token and transform into a post token
handleClose :: Configuration -> Ref (Map PrimaryKey WebSocketConnection) -> Request -> CloseCode -> CloseReason -> Effect Unit
handleClose (Configuration configuration) allConnections request _ _ = pure unit

--REFACTOR: untangle the im logic from the websocket logic
handleMessage ::  WebSocketPayloadServer -> WebSocketEffect
handleMessage payload = do
      { connection, sessionUserID, allConnections } <- RR.ask
      case payload of
            Connect -> R.liftEffect $ ER.modify_ (DM.insert sessionUserID connection) allConnections
            ReadMessages { ids } -> SID.markRead sessionUserID ids
            ToBlock { id } -> do
                  possibleConnection <- R.liftEffect (DM.lookup id <$> ER.read allConnections)
                  whenJust possibleConnection $ \recipientConnection -> sendWebSocketMessage recipientConnection $ BeenBlocked { id: sessionUserID }
            ServerMessage {id, userID: recipient, content, turn} -> do
                  date <- R.liftEffect $ map MDateTime EN.nowDateTime
                  Tuple messageID finalContent <- SIA.insertMessage sessionUserID recipient content
                  sendWebSocketMessage connection $ Received {
                        previousID: id,
                        id: messageID,
                        userID: sessionUserID
                  }

                  possibleRecipientConnection <- R.liftEffect (DM.lookup recipient <$> ER.read allConnections)
                  whenJust possibleRecipientConnection $ \recipientConnection ->
                        sendWebSocketMessage recipientConnection $ ClientMessage {
                              id : messageID,
                              userID: sessionUserID,
                              content: finalContent,
                              date
                        }
                  --pass along karma calculation to wheel
                  whenJust turn (R.liftEffect <<< SWL.sendMessage sessionUserID recipient)
      where whenJust :: forall v. Maybe v -> (v -> WebSocketEffect) -> WebSocketEffect
            whenJust value f = case value of
                  Nothing -> pure unit
                  Just v -> f v

handleConnection :: Configuration -> Pool -> Ref (Map PrimaryKey WebSocketConnection) -> WebSocketConnection -> Request -> Effect Unit
handleConnection c@(Configuration configuration) pool allConnections connection request = do
      SW.onError connection handleError
      SW.onClose connection $ handleClose c allConnections request
      SW.onMessage connection runMessageHandler
      where runMessageHandler (WebSocketMessage message) = do
                  let WebSocketTokenPayloadServer token payload = PU.unsafePartial (DE.fromRight $ SJ.fromJSON message)
                  maybeUserID <- ST.userIDFromToken configuration.tokenSecretPOST token
                  case maybeUserID of
                        Nothing -> do
                              SW.close connection
                              EC.log "closed due to auth error"
                        Just sessionUserID -> do
                              let run = R.runBaseAff' <<< RE.catch (reportError payload) <<< RR.runReader { allConnections, pool, sessionUserID, connection } $ handleMessage payload
                              EA.launchAff_ $ run `CMEC.catchError` (reportError payload)
            reportError :: forall a b. MonadEffect b => WebSocketPayloadServer -> a -> b Unit
            reportError = const <<< sendWebSocketMessage connection <<< PayloadError

sendWebSocketMessage :: forall b. MonadEffect b => WebSocketConnection -> WebSocketPayloadClient -> b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJSON