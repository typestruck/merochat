module Server.WebSocket.Events where

import Prelude
import Server.Types
import Shared.ContentType
import Shared.Experiments.Types
import Shared.IM.Types
import Shared.User

import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DDT
import Data.Either (Either(..))
import Data.Enum as DEN
import Data.Foldable as DF
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.Time.Duration (Minutes)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Foreign as F
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
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..), AliveWebSocketConnection)
import Server.WebSocket as SW
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SDT
import Shared.Json as SJ
import Shared.Path (updateHash)
import Shared.ResponseError (DatabaseError, ResponseError(..))
import Shared.Unsafe as SU
import Simple.JSON (class WriteForeign, writeJSON)

type WebSocketEffect = BaseEffect WebSocketReader Unit

type WebSocketReader = BaseReader
      ( sessionUserId ∷ Int
      , configuration ∷ Configuration
      , allConnections ∷ Ref (HashMap Int AliveWebSocketConnection)
      , availability ∷ Ref (HashMap Int Availability)
      )

type WebSocketReaderLite = BaseReader
      ( availability ∷ Ref (HashMap Int Availability)
      )

newtype DT = DT DateTime

instance Newtype DT DateTime

instance WriteForeign DT where
      writeImpl (DT (DateTime dt (Time h m s ms))) = F.unsafeToForeign (SDT.formatISODate' dt <> "t" <> time <> "+0000")
            where
            time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> "." <> show (DEN.fromEnum ms)

aliveDelay ∷ Int
aliveDelay = 1000 * 60 * aliveDelayMinutes

aliveDelayMinutes ∷ Int
aliveDelayMinutes = 5

handleConnection ∷ Configuration → Pool → Ref (HashMap Int AliveWebSocketConnection) → Ref (HashMap Int Availability) → WebSocketConnection → Request → Effect Unit
handleConnection configuration@{ tokenSecret } pool allConnections availability connection request = do
      maybeUserID ← ST.userIDFromToken tokenSecret <<< DM.fromMaybe "" $ do
            uncooked ← FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked
      case maybeUserID of
            Nothing → do
                  SW.terminate connection
                  EC.log "terminated due to auth error"
            Just sessionUserId → do
                  now ← EN.nowDateTime
                  ER.modify_ (DH.insert sessionUserId { lastSeen: now, connection }) allConnections
                  ER.modify_ (DH.insert sessionUserId Online) availability
                  SW.onError connection handleError
                  SW.onClose connection (handleClose allConnections availability sessionUserId)
                  SW.onMessage connection (runMessageHandler sessionUserId)
      where
      runMessageHandler sessionUserId (WebSocketMessage message) = do
            case SJ.fromJSON message of
                  Right payload → do
                        let run = R.runBaseAff' <<< RE.catch (\e → reportError payload (checkInternalError e) e) <<< RR.runReader { allConnections, configuration, pool, sessionUserId, availability } $ handleMessage payload
                        EA.launchAff_ <<< EA.catchError run $ reportError payload Nothing
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

handleClose ∷ Ref (HashMap Int AliveWebSocketConnection) → Ref (HashMap Int Availability) → Int → CloseCode → CloseReason → Effect Unit
handleClose allConnections availability sessionUserId _ _ = do
      now ← EN.nowDateTime
      ER.modify_ (DH.alter (updateAvailability false now) sessionUserId) availability
      ER.modify_ (DH.delete sessionUserId) allConnections

handleMessage ∷ WebSocketPayloadServer → WebSocketEffect
handleMessage payload = do
      reading@{ sessionUserId, allConnections } ← RR.ask
      { connection } ← liftEffect (SU.fromJust <<< DH.lookup sessionUserId <$> ER.read allConnections)
      case payload of
            UpdateHash → sendUpdatedHash connection
            Typing { id } → sendTyping allConnections sessionUserId id
            Ping ping → sendPing reading ping connection
            ChangeStatus change → sendStatusChange allConnections change sessionUserId
            UnavailableFor { id } → sendUnavailability allConnections sessionUserId id
            OutgoingMessage message → sendOutgoingMessage allConnections message connection sessionUserId

sendUpdatedHash ∷ WebSocketConnection → WebSocketEffect
sendUpdatedHash connection = sendWebSocketMessage connection <<< Content $ CurrentHash updateHash

sendTyping ∷ Ref (HashMap Int AliveWebSocketConnection) → Int → Int → WebSocketEffect
sendTyping allConnections sessionUserId userId = do
      possibleConnection ← liftEffect (DH.lookup userId <$> ER.read allConnections)
      whenJust possibleConnection $ \{ connection } → sendWebSocketMessage connection <<< Content $ ContactTyping { id: sessionUserId }

--pings fulfill three purposes
-- keep the connection alive
-- maintain online status
-- get contacts/suggestions online status
sendPing ∷ WebSocketReader → { isActive ∷ Boolean, statusFor ∷ Array Int } → WebSocketConnection → WebSocketEffect
sendPing { allConnections, availability, sessionUserId } { isActive, statusFor } connection = do
      possibleConnection ← R.liftEffect (DH.lookup sessionUserId <$> ER.read allConnections)
      if DM.isNothing possibleConnection then
            --shouldn't be possible 🤔
            liftEffect do
                  EC.log "ping without saved connection"
                  SW.terminate connection
      else do

            avl ← liftEffect $ ER.read availability
            Tuple status missing ← makeAvailability avl
            liftEffect do
                  now ← EN.nowDateTime
                  ER.modify_ (DH.insert sessionUserId { connection, lastSeen: now }) allConnections
                  ER.modify_ (DH.alter (updateAvailability isActive now) sessionUserId) availability
                  ER.modify_ (\avlb → DA.foldl updateMissingAvailability avlb missing) availability
                  sendWebSocketMessage connection $ Pong { status }
      where
      makeAvailability avl = do
            let
                  statuses = map (\id → { id, status: DM.fromMaybe None $ DH.lookup id avl }) statusFor
                  nones = map _.id $ DA.filter ((None == _) <<< _.status) statuses
            if DA.null nones then
                  pure $ Tuple statuses []
            else do
                  lastSeens ← DH.fromArrayBy _.who _.date <$> SID.queryLastSeen nones
                  let
                        records = map
                              ( \{ id, status } →
                                      { id
                                      , status: case status of
                                              None → DM.maybe None (LastSeen <<< DateTimeWrapper) $ DH.lookup id lastSeens
                                              _ → status
                                      }
                              )
                              statuses
                  pure <<< Tuple records $ DH.toArrayBy Tuple lastSeens

      updateMissingAvailability hashMap (Tuple id date) = DH.insert id (LastSeen $ DateTimeWrapper date) hashMap

sendStatusChange ∷ Ref (HashMap Int AliveWebSocketConnection) → { ids ∷ Array Int, persisting ∷ Boolean, status ∷ MessageStatus, userId ∷ Int } → Int → WebSocketEffect
sendStatusChange allConnections { userId, status, ids, persisting } sessionUserId = do
      when persisting $ SID.changeStatus sessionUserId status ids
      possibleSenderConnection ← R.liftEffect (DH.lookup userId <$> ER.read allConnections)
      whenJust possibleSenderConnection $ \{ connection } →
            sendWebSocketMessage connection <<< Content $ ServerChangedStatus
                  { ids
                  , status
                  , userId: sessionUserId
                  }

sendUnavailability ∷ Ref (HashMap Int AliveWebSocketConnection) → Int → Int → WebSocketEffect
sendUnavailability allConnections sessionUserId userId = do
      possibleConnection ← R.liftEffect (DH.lookup userId <$> ER.read allConnections)
      whenJust possibleConnection $ \{ connection } → sendWebSocketMessage connection <<< Content $ ContactUnavailable
            { userId: sessionUserId
            , temporaryMessageId: Nothing
            }

sendOutgoingMessage ∷ Ref (HashMap Int AliveWebSocketConnection) → OutgoingRecord → WebSocketConnection → Int → WebSocketEffect
sendOutgoingMessage allConnections { id: temporaryId, userId, content, turn, experimenting } connection sessionUserId = do
      date ← R.liftEffect $ map DateTimeWrapper EN.nowDateTime
      processed ← case experimenting of
            --impersonating experiment messages are not saved
            Just (ImpersonationPayload _) → do
                  msg ← SIA.processMessageContent content
                  pure <<< Just $ Tuple temporaryId msg
            _ →
                  SIA.processMessage sessionUserId userId temporaryId content
      case processed of
            Just (Tuple messageId finalContent) → do
                  sendWebSocketMessage connection <<< Content $ ServerReceivedMessage
                        { previousId: temporaryId
                        , id: messageId
                        , userId
                        }
                  possibleRecipientConnection ← R.liftEffect (DH.lookup userId <$> ER.read allConnections)
                  whenJust possibleRecipientConnection $ \{ connection: recipientConnection } →
                        sendWebSocketMessage recipientConnection <<< Content $ NewIncomingMessage
                              { id: messageId
                              , userId: sessionUserId
                              , content: finalContent
                              , experimenting: experimenting
                              , date
                              }
                  --pass along karma calculation to wheel
                  whenJust turn (SIA.processKarma sessionUserId userId)
            --meaning recipient can't be messaged
            Nothing →
                  sendWebSocketMessage connection <<< Content $ ContactUnavailable { userId, temporaryMessageId: Just temporaryId }

whenJust ∷ ∀ v. Maybe v → (v → WebSocketEffect) → WebSocketEffect
whenJust value f = case value of
      Nothing → pure unit
      Just v → f v

updateAvailability ∷ Boolean → DateTime → Maybe Availability → Maybe Availability
updateAvailability isActive date avl
      | isActive = Just Online
      | otherwise = case avl of
              ls@(Just (LastSeen _)) → ls
              _ → Just <<< LastSeen $ DateTimeWrapper date

sendWebSocketMessage ∷ ∀ b. MonadEffect b ⇒ WebSocketConnection → FullWebSocketPayloadClient → b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson

-- | Connections are dropped after 5 minutes of inactivity
checkLastSeen ∷ Ref (HashMap Int AliveWebSocketConnection) → Ref (HashMap Int Availability) → Effect Unit
checkLastSeen allConnections availability = do
      connections ← ER.read allConnections
      now ← EN.nowDateTime
      DF.traverse_ (check now) $ DH.toArrayBy Tuple connections
      where
      check now (Tuple id { lastSeen, connection })
            | hasExpired lastSeen now = do
                    ER.modify_ (DH.delete id) allConnections
                    ER.modify_ (DH.insert id (LastSeen $ DateTimeWrapper lastSeen)) availability
                    SW.terminate connection
            | otherwise = pure unit

      hasExpired lastSeen now = aliveDelayMinutes <= DI.floor (DN.unwrap (DDT.diff now lastSeen ∷ Minutes))

--have to decide if online needs to be serialized
persistLastSeen ∷ WebSocketReaderLite → Effect Unit
persistLastSeen reading@{ availability } = do
      availabilities ← ER.read availability
      when (not $ DH.isEmpty availabilities) do
            let run = R.runBaseAff' <<< RE.catch (const (pure unit)) <<< RR.runReader reading <<< SID.upsertLastSeen <<< writeJSON <<< DA.catMaybes $ DH.toArrayBy lastSeens availabilities
            EA.launchAff_ $ EA.catchError run logError
      where
      lastSeens id = case _ of
            LastSeen (DateTimeWrapper date) → Just { who: id, date: DT date }
            _ → Nothing

      logError = liftEffect <<< EC.logShow
