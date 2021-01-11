module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.Types

import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
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
import Foreign.Object as FO
import Node.HTTP (Request)
import Node.HTTP as NH
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Cookies (cookieName)
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.Token as ST
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.JSON as SJ

handleClose ::  Ref (HashMap PrimaryKey WebSocketConnection) -> PrimaryKey -> CloseCode -> CloseReason -> Effect Unit
handleClose allConnections id _ _ = ER.modify_ (DH.delete id) allConnections

--REFACTOR: untangle the im logic from the websocket logic
handleMessage ::  WebSocketPayloadServer -> WebSocketEffect
handleMessage payload = do
      { connection, sessionUserID, allConnections } <- RR.ask
      case payload of
            Connect -> R.liftEffect $ ER.modify_ (DH.insert sessionUserID connection) allConnections
            ReadMessages { ids } -> SID.markRead sessionUserID ids
            ToBlock { id } -> do
                  possibleConnection <- R.liftEffect (DH.lookup id <$> ER.read allConnections)
                  whenJust possibleConnection $ \recipientConnection -> sendWebSocketMessage recipientConnection $ BeenBlocked { id: sessionUserID }
            OutgoingMessage { id: temporaryID , userID: recipient, content, turn } -> do
                  date <- R.liftEffect $ map DateTimeWrapper EN.nowDateTime
                  Tuple messageID finalContent <- SIA.processMessage sessionUserID recipient temporaryID content
                  sendWebSocketMessage connection $ ServerReceivedMessage {
                        previousID: temporaryID,
                        id: messageID,
                        userID: sessionUserID
                  }

                  possibleRecipientConnection <- R.liftEffect (DH.lookup recipient <$> ER.read allConnections)
                  whenJust possibleRecipientConnection $ \recipientConnection ->
                        sendWebSocketMessage recipientConnection $ NewIncomingMessage {
                              id : messageID,
                              userID: sessionUserID,
                              content: finalContent,
                              date
                        }
                  --pass along karma calculation to wheel
                  whenJust turn (SIA.processKarma sessionUserID recipient)
      where whenJust :: forall v. Maybe v -> (v -> WebSocketEffect) -> WebSocketEffect
            whenJust value f = case value of
                  Nothing -> pure unit
                  Just v -> f v

handleConnection :: Configuration -> Pool -> Ref (HashMap PrimaryKey WebSocketConnection) -> WebSocketConnection -> Request -> Effect Unit
handleConnection c@{ tokenSecret } pool allConnections connection request = do
      maybeUserID <- ST.userIDFromToken tokenSecret <<< DM.fromMaybe "" $ do
            uncooked <- FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _ ) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked
      case maybeUserID of
            Nothing -> do
                  SW.close connection
                  EC.log "closed due to auth error"
            Just sessionUserID -> do
                  SW.onError connection handleError
                  SW.onClose connection (handleClose allConnections sessionUserID)
                  SW.onMessage connection (runMessageHandler sessionUserID)
      where runMessageHandler sessionUserID (WebSocketMessage message) = do
                  case SJ.fromJSON message of
                        Right payload -> do
                              let run = R.runBaseAff' <<< RE.catch (\e -> reportError payload (checkInternalError e) e) <<< RR.runReader { allConnections, pool, sessionUserID, connection } $ handleMessage payload
                              EA.launchAff_ $ run `CMEC.catchError` (reportError payload Nothing)
                        Left error -> do
                              SW.close connection
                              EC.log $ "closed due to seriliazation error: " <> error

            reportError :: forall a b. MonadEffect b => WebSocketPayloadServer -> Maybe DatabaseError -> a -> b Unit
            reportError origin context _ = sendWebSocketMessage connection $ PayloadError { origin, context }

            checkInternalError = case _ of
                  InternalError { context } -> context
                  _ -> Nothing

sendWebSocketMessage :: forall b. MonadEffect b => WebSocketConnection -> WebSocketPayloadClient -> b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJSON

handleError :: Error -> Effect Unit
handleError = EC.log <<< show