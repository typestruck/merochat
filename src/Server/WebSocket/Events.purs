module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.ContentType
import Shared.Experiments.Types
import Shared.IM.Types

import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.DateTime (DateTime)
import Data.DateTime as DDT
import Data.Either (Either(..))
import Data.Foldable as DF
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Time.Duration (Minutes)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Droplet.Driver (Pool)
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
import Server.WebSocket (CloseCode, CloseReason, AliveWebSocketConnection, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.JSON as SJ
import Shared.Path (updateHash)
import Shared.ResponseError (DatabaseError, ResponseError(..))

type WebSocketEffect = BaseEffect WebSocketReader Unit

type WebSocketReader = BaseReader
      ( sessionUserID ∷ Int
      , connection ∷ WebSocketConnection
      , allConnections ∷ Ref (HashMap Int AliveWebSocketConnection)
      , availability :: Ref (HashMap Int Availability)
      )

data Availability = Online | LastSeen DateTime
aliveDelay ∷ Int
aliveDelay = 1000 * 60 * aliveDelayMinutes

aliveDelayMinutes ∷ Int
aliveDelayMinutes = 10

handleConnection ∷ Configuration → Pool → Ref (HashMap Int AliveWebSocketConnection) → Ref StorageDetails → Ref (HashMap Int Availability) -> WebSocketConnection → Request → Effect Unit
handleConnection { tokenSecret } pool allConnections storageDetails availability connection request = do
      maybeUserID ← ST.userIDFromToken tokenSecret <<< DM.fromMaybe "" $ do
            uncooked ← FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked
      case maybeUserID of
            Nothing → do
                  SW.terminate connection
                  EC.log "terminated due to auth error"
            Just sessionUserID → do
                  now ← EN.nowDateTime
                  ER.modify_ (DH.insert sessionUserID { lastSeen: now, connection }) allConnections
                  ER.modify_ (DH.insert sessionUserID Online) availability
                  SW.onError connection handleError
                  SW.onClose connection (handleClose allConnections sessionUserID)
                  SW.onMessage connection (runMessageHandler sessionUserID)
      where
      runMessageHandler sessionUserID (WebSocketMessage message) = do
            case SJ.fromJSON message of
                  Right payload → do
                        let run = R.runBaseAff' <<< RE.catch (\e → reportError payload (checkInternalError e) e) <<< RR.runReader { storageDetails, allConnections, pool, sessionUserID, connection, availability } $ handleMessage payload
                        EA.launchAff_ $ run `CMEC.catchError` (reportError payload Nothing)
                  Left error → do
                        SW.terminate connection
                        EC.log $ "terminated due to serialization error: " <> error

      reportError ∷ ∀ a b. MonadEffect b ⇒ WebSocketPayloadServer → Maybe DatabaseError → a → b Unit
      reportError origin context _ = sendWebSocketMessage connection <<< Content $ PayloadError { origin, context }

      checkInternalError = case _ of
            InternalError { context } → context
            _ → Nothing

handleError ∷ Error → Effect Unit
handleError = EC.log <<< show

handleClose ∷ Ref (HashMap Int AliveWebSocketConnection) → Int → CloseCode → CloseReason → Effect Unit
handleClose allConnections id _ _ = ER.modify_ (DH.delete id) allConnections

--REFACTOR: untangle the im logic from the websocket logic
handleMessage ∷ WebSocketPayloadServer → WebSocketEffect
handleMessage payload = do
      { connection, sessionUserID, allConnections, availability } ← RR.ask
      case payload of
            UpdateHash →
                  sendWebSocketMessage connection <<< Content $ CurrentHash updateHash
            Typing { id  } -> do
                  possibleConnection ← R.liftEffect (DH.lookup id <$> ER.read allConnections)
                  whenJust possibleConnection $ \{ connection: recipientConnection } → sendWebSocketMessage recipientConnection <<< Content $ ContactTyping { id: sessionUserID }
            Ping { isActive } → do
                  possibleConnection ← R.liftEffect (DH.lookup sessionUserID <$> ER.read allConnections)
                  if DM.isNothing possibleConnection then
                        --shouldn't be possible 🤔
                        R.liftEffect $ do
                              EC.log "ping without saved connection"
                              SW.terminate connection
                              ER.modify_ (DH.delete sessionUserID) availability
                  else
                        R.liftEffect $ do
                              now ← EN.nowDateTime
                              ER.modify_ (DH.update (Just <<< (_ { lastSeen = now })) sessionUserID) allConnections
                              ER.modify_ (DH.alter (updateAvailability isActive now) sessionUserID) availability
                              sendWebSocketMessage connection Pong
            ChangeStatus { userID: sender, status, ids, persisting } → do
                  when persisting $ SID.changeStatus sessionUserID status ids
                  possibleSenderConnection ← R.liftEffect (DH.lookup sender <$> ER.read allConnections)
                  whenJust possibleSenderConnection $ \{ connection: senderConnection } →
                        sendWebSocketMessage senderConnection <<< Content $ ServerChangedStatus
                              { ids
                              , status
                              , userID: sessionUserID
                              }
            UnavailableFor { id } → do
                  possibleConnection ← R.liftEffect (DH.lookup id <$> ER.read allConnections)
                  whenJust possibleConnection $ \{ connection: recipientConnection } → sendWebSocketMessage recipientConnection <<< Content $ ContactUnavailable { id: sessionUserID }
            OutgoingMessage { id: temporaryID, userID: recipient, content, turn, experimenting } → do
                  date ← R.liftEffect $ map DateTimeWrapper EN.nowDateTime
                  Tuple messageID finalContent ← case experimenting of
                        --impersonating experiment messages are not saved
                        Just (ImpersonationPayload _) → do
                              msg ← SIA.processMessageContent content
                              pure $ Tuple temporaryID msg
                        _ →
                              SIA.processMessage sessionUserID recipient temporaryID content
                  sendWebSocketMessage connection <<< Content $ ServerReceivedMessage
                        { previousID: temporaryID
                        , id: messageID
                        , userID: recipient
                        }

                  possibleRecipientConnection ← R.liftEffect (DH.lookup recipient <$> ER.read allConnections)
                  whenJust possibleRecipientConnection $ \{ connection: recipientConnection } →
                        sendWebSocketMessage recipientConnection <<< Content $ NewIncomingMessage
                              { id: messageID
                              , userID: sessionUserID
                              , content: finalContent
                              , experimenting: experimenting
                              , date
                              }
                  --pass along karma calculation to wheel
                  whenJust turn (SIA.processKarma sessionUserID recipient)
      where
      whenJust ∷ ∀ v. Maybe v → (v → WebSocketEffect) → WebSocketEffect
      whenJust value f = case value of
            Nothing → pure unit
            Just v → f v

      updateAvailability isActive date av
            | isActive = Just Online
            | otherwise = case av of
                  ls@(Just (LastSeen _)) -> ls
                  _ -> Just $ LastSeen date

sendWebSocketMessage ∷ ∀ b. MonadEffect b ⇒ WebSocketConnection → FullWebSocketPayloadClient → b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJSON

checkLastSeen ∷ Ref (HashMap Int AliveWebSocketConnection) → Effect Unit
checkLastSeen allConnections = do
      connections ← ER.read allConnections
      now ← EN.nowDateTime
      DF.traverse_ (check now) $ DH.toArrayBy Tuple connections
      where
      check now (Tuple id { lastSeen, connection })
            | hasExpired lastSeen now = do
                    ER.modify_ (DH.delete id) allConnections
                    SW.terminate connection
            | otherwise = pure unit

      hasExpired lastSeen now = aliveDelayMinutes <= DI.floor (DN.unwrap (DDT.diff now lastSeen ∷ Minutes))
