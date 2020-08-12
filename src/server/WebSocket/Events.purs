module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Server.IM.Action as SIA
import Node.HTTP (Request)
import Run as R
import Run.Except as RE
import Run.Reader as RR
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
handleMessage :: WebSocketConnection -> WebSocketMessage -> WebSocketEffect
handleMessage connection (WebSocketMessage message) = do
      possiblePayload <- pure $ SJ.fromJSON message
      case possiblePayload of
            Right payload -> do
                  { allConnections } <- RR.ask
                  case payload of
                        Connect token -> withUser token $ \userID -> R.liftEffect $ ER.modify_ (DM.insert userID connection) allConnections
                        ReadMessages { token, ids } -> withUser token $ \userID -> SID.markRead userID ids
                        ServerMessage {id, userID: recipient, token, content, turn} -> withUser token $ \userID -> do
                              date <- R.liftEffect $ map MDateTime EN.nowDateTime
                              Tuple messageID finalContent <- SIA.insertMessage userID recipient content
                              sendMessage connection <<< SJ.toJSON $ Received {
                                    previousID: id,
                                    id: messageID
                              }

                              possibleRecipientConnection <- R.liftEffect (DM.lookup recipient <$> ER.read allConnections)
                              whenJust possibleRecipientConnection $ \recipientConnection ->
                                    sendMessage recipientConnection <<< SJ.toJSON $ ClientMessage {
                                          id : messageID,
                                          userID,
                                          content: finalContent,
                                          date
                                    }
                              --pass along karma calculation to wheel
                              whenJust turn (R.liftEffect <<< SWL.sendMessage userID recipient)
            Left error -> do
                  log $ "received faulty payload " <> error

      where log = R.liftEffect <<< EC.log
            sendMessage connection' = R.liftEffect <<< SW.sendMessage connection' <<< WebSocketMessage
            withUser token f = do
                  {configuration: Configuration configuration} <- RR.ask
                  user <- R.liftEffect $ ST.userIDFromToken configuration.tokenSecretPOST token
                  case user of
                              Nothing -> do
                                    R.liftEffect $ SW.close connection
                                    log "closed due to auth error"
                              Just userId -> f userId
            whenJust :: forall v. Maybe v -> (v -> WebSocketEffect) -> WebSocketEffect
            whenJust value f = case value of
                  Nothing -> pure unit
                  Just v -> f v

handleConnection :: WebSocketReader -> WebSocketConnection -> Request -> Effect Unit
handleConnection reading connection request = do
      SW.onError connection handleError
      SW.onClose connection $ handleClose reading.configuration reading.allConnections request
      SW.onMessage connection
            (EA.launchAff_ <<<
             R.runBaseAff' <<<
             RE.catch (const (pure unit)) <<<
             RR.runReader reading <<<
             handleMessage connection)