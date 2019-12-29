module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Parser as DAC
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as ER
import Node.HTTP (Request)
import Server.Token as ST
import Server.WebSocket (WebSocketConnection, WebSocketMessage(..), CloseCode, CloseReason)
import Server.WebSocket as SW

handleError :: Error -> Effect Unit
handleError = EC.log <<< show

--here it seems like the only way is get the cookie token and transform into a post token
handleClose :: Configuration -> Ref (Map String WebSocketConnection) -> Request -> CloseCode -> CloseReason -> Effect Unit
handleClose (Configuration configuration) allConnections request _ _ = do
        EC.log "closed from event"

handleMessage :: Configuration -> Ref (Map String WebSocketConnection) -> WebSocketConnection -> WebSocketMessage -> Effect Unit
handleMessage (Configuration configuration) allConnections connection (WebSocketMessage message) = void do
        possiblePayload <- pure parse
        case possiblePayload of
                Right payload ->
                        case payload of
                                Connect token -> withUser token $ \userID -> ER.modify_ (DM.insert (show userID) connection) allConnections
                                Message {id, user: recipient, token, content} -> withUser token $ \userId -> do
                                        EC.log content
                                e -> EC.log $ "received bogus payload " <> show e
                Left error -> do
                        EC.log $ "received faulty payload " <> error

        where   withUser token f = do
                        user <- ST.userIDFromToken configuration.tokenSecretPOST token
                        (case user of
                                Nothing -> do
                                        SW.close connection
                                        EC.log "closed due to auth error"
                                Just userId -> f userId)

                parse = do
                        json <- DAC.jsonParser message
                        DADGR.genericDecodeJson json

handleConnection :: Configuration -> Ref (Map String WebSocketConnection) -> WebSocketConnection -> Request -> Effect Unit
handleConnection configuration allConnections connection request = do
        EC.log "connecting"
        SW.onMessage connection $ handleMessage configuration allConnections connection
        SW.onError connection handleError
        SW.onClose connection $ handleClose configuration allConnections request