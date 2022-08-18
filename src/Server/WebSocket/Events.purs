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
import Data.Tuple as DT
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
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SDT
import Shared.Json as SJ
import Shared.Path (updateHash)
import Shared.ResponseError (DatabaseError, ResponseError(..))
import Shared.Unsafe as SU
import Simple.JSON (class WriteForeign, writeJSON)

type UserAvailability =
      { connection ∷ Maybe WebSocketConnection
      , lastSeen ∷ DateTime
      , availability ∷ Availability
      }

type WebSocketEffect = BaseEffect WebSocketReader Unit

type WebSocketReader = BaseReader
      ( sessionUserId ∷ Int
      , configuration ∷ Configuration
      , userAvailability ∷ Ref (HashMap Int UserAvailability)
      )

type WebSocketReaderLite = BaseReader
      ( userAvailability ∷ Ref (HashMap Int UserAvailability)
      )

newtype DT = DT DateTime

instance Newtype DT DateTime

instance WriteForeign DT where
      writeImpl (DT (DateTime dt (Time h m s ms))) = F.unsafeToForeign (SDT.formatIsoDate' dt <> "t" <> time <> "+0000")
            where
            time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> "." <> show (DEN.fromEnum ms)

aliveDelay ∷ Int
aliveDelay = 1000 * 60 * aliveDelayMinutes

aliveDelayMinutes ∷ Int
aliveDelayMinutes = 5

handleConnection ∷ Configuration → Pool → Ref (HashMap Int UserAvailability) → WebSocketConnection → Request → Effect Unit
handleConnection configuration@{ tokenSecret } pool userAvailability connection request = do
      maybeUserID ← ST.userIDFromToken tokenSecret <<< DM.fromMaybe "" $ do
            uncooked ← FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked
      case maybeUserID of
            Nothing → do
                  SW.terminate connection
                  EC.log "terminated due to auth error"
            Just sessionUserId → do
                  now ← EN.nowDateTime
                  ER.modify_
                        ( DH.insert sessionUserId
                                { lastSeen: now
                                , connection: Just connection
                                , availability: Online
                                }
                        )
                        userAvailability
                  SW.onError connection handleError
                  SW.onClose connection (handleClose userAvailability sessionUserId)
                  SW.onMessage connection (runMessageHandler sessionUserId)
      where
      runMessageHandler sessionUserId (WebSocketMessage message) = do
            case SJ.fromJSON message of
                  Right payload → do
                        let run = R.runBaseAff' <<< RE.catch (\e → reportError payload (checkInternalError e) e) <<< RR.runReader { userAvailability, configuration, pool, sessionUserId } $ handleMessage payload
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

handleClose ∷ Ref (HashMap Int UserAvailability) → Int → CloseCode → CloseReason → Effect Unit
handleClose userAvailability sessionUserId _ _ = do
      now ← EN.nowDateTime
      ER.modify_ (DH.insert sessionUserId (updateUserAvailability false now Nothing)) userAvailability

handleMessage ∷ WebSocketPayloadServer → WebSocketEffect
handleMessage payload = do
      reading@{ sessionUserId, userAvailability } ← RR.ask
      userAvl ← liftEffect (SU.fromJust <<< DH.lookup sessionUserId <$> ER.read userAvailability)
      let connection = SU.fromJust userAvl.connection
      case payload of
            UpdateHash → sendUpdatedHash connection
            Typing { id } → sendTyping userAvailability sessionUserId id
            Ping ping → sendPing reading ping connection
            ChangeStatus change → sendStatusChange userAvailability change sessionUserId
            UnavailableFor { id } → sendUnavailability userAvailability sessionUserId id
            OutgoingMessage message → sendOutgoingMessage userAvailability message connection sessionUserId

sendUpdatedHash ∷ WebSocketConnection → WebSocketEffect
sendUpdatedHash connection = sendWebSocketMessage connection <<< Content $ CurrentHash updateHash

sendTyping ∷ Ref (HashMap Int UserAvailability) → Int → Int → WebSocketEffect
sendTyping userAvailability sessionUserId userId = do
      possibleConnection ← liftEffect (DH.lookup userId <$> ER.read userAvailability)
      whenJust possibleConnection $ \connection → sendWebSocketMessage connection <<< Content $ ContactTyping { id: sessionUserId }

--pings fulfill three purposes
-- keep the connection alive
-- maintain online status
-- get contacts/suggestions online status
sendPing ∷ WebSocketReader → { isActive ∷ Boolean, statusFor ∷ Array Int } → WebSocketConnection → WebSocketEffect
sendPing { userAvailability, sessionUserId } { isActive, statusFor } connection = do
      possibleConnection ← R.liftEffect (DH.lookup sessionUserId <$> ER.read userAvailability)
      if DM.isNothing possibleConnection then
            --shouldn't be possible 🤔
            liftEffect do
                  EC.log "ping without saved connection"
                  SW.terminate connection
      else do
            avl ← liftEffect $ ER.read userAvailability
            Tuple status missing ← makeAvailability avl
            liftEffect do
                  now ← EN.nowDateTime
                  ER.modify_ (DH.insert sessionUserId (updateUserAvailability isActive now (Just connection)) ) userAvailability
                  ER.modify_ (\avlb → DA.foldl updateMissingAvailability avlb missing) userAvailability
                  sendWebSocketMessage connection $ Pong { status }
      where
      makeAvailability avl = do
            let
                  statuses = map (\id → { id, status: DM.maybe None _.availability $ DH.lookup id avl }) statusFor
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

      updateMissingAvailability hashMap (Tuple id date) =
            DH.alter
                  ( Just <<< case _ of
                          Nothing → { connection: Nothing, lastSeen: date, availability: LastSeen $ DateTimeWrapper date }
                          Just avl → avl { lastSeen = date, availability = LastSeen $ DateTimeWrapper date }
                  )
                  id
                  hashMap

sendStatusChange ∷ Ref (HashMap Int UserAvailability) → { ids ∷ Array (Tuple Int (Array Int)), persisting ∷ Boolean, status ∷ MessageStatus } → Int → WebSocketEffect
sendStatusChange userAvailability { status, ids, persisting } sessionUserId = do
      when persisting <<< SID.changeStatus sessionUserId status $ DA.concatMap DT.snd ids
      DF.traverse_ send ids
      where send (Tuple userId messageIds) = do
                  possibleSenderConnection ← R.liftEffect (DH.lookup userId <$> ER.read userAvailability)
                  whenJust possibleSenderConnection $ \connection →
                        sendWebSocketMessage connection <<< Content $ ServerChangedStatus
                              { ids: messageIds
                              , status
                              , userId: sessionUserId
                              }

sendUnavailability ∷ Ref (HashMap Int UserAvailability) → Int → Int → WebSocketEffect
sendUnavailability userAvailability sessionUserId userId = do
      possibleConnection ← R.liftEffect (DH.lookup userId <$> ER.read userAvailability)
      whenJust possibleConnection $ \connection → sendWebSocketMessage connection <<< Content $ ContactUnavailable
            { userId: sessionUserId
            , temporaryMessageId: Nothing
            }

sendOutgoingMessage ∷ Ref (HashMap Int UserAvailability) → OutgoingRecord → WebSocketConnection → Int → WebSocketEffect
sendOutgoingMessage userAvailability { id: temporaryId, userId, content, turn, experimenting } connection sessionUserId = do
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
                  possibleRecipientConnection ← R.liftEffect (DH.lookup userId <$> ER.read userAvailability)
                  whenJust possibleRecipientConnection $ \ recipientConnection →
                        sendWebSocketMessage recipientConnection <<< Content $ NewIncomingMessage
                              { id: messageId
                              , userId: sessionUserId
                              , content: finalContent
                              , experimenting: experimenting
                              , date
                              }
                  --pass along karma calculation to wheel
                  DM.maybe (pure unit) (SIA.processKarma sessionUserId userId) turn
            --meaning recipient can't be messaged
            Nothing →
                  sendWebSocketMessage connection <<< Content $ ContactUnavailable { userId, temporaryMessageId: Just temporaryId }

whenJust ∷ ∀ r. Maybe { connection ∷ Maybe WebSocketConnection | r } → (WebSocketConnection → WebSocketEffect) → WebSocketEffect
whenJust value handler = do
      let
            connection = do
                  v ← value
                  v.connection
      case connection of
            Nothing → pure unit
            Just c → handler c

updateUserAvailability ∷ Boolean → DateTime → Maybe WebSocketConnection → UserAvailability
updateUserAvailability isActive date connection =
      { connection
      , lastSeen: date
      , availability: if isActive then Online else LastSeen $ DateTimeWrapper date
      }

sendWebSocketMessage ∷ ∀ b. MonadEffect b ⇒ WebSocketConnection → FullWebSocketPayloadClient → b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson

-- | Connections are dropped after 5 minutes of inactivity
checkLastSeen ∷ Ref (HashMap Int UserAvailability) → Effect Unit
checkLastSeen userAvailability = do
      now ← EN.nowDateTime
      availabilities ← ER.read userAvailability
      DF.traverse_ (check now) $ DH.toArrayBy Tuple availabilities
      where
      check now (Tuple id { lastSeen, connection })
            | hasExpired lastSeen now = do
                    ER.modify_ (DH.insert id (updateUserAvailability false lastSeen Nothing)) userAvailability
                    DM.maybe (pure unit) SW.terminate connection
            | otherwise = pure unit

      hasExpired lastSeen now = aliveDelayMinutes <= DI.floor (DN.unwrap (DDT.diff now lastSeen ∷ Minutes))

-- | Last seen dates are serialized every 5 minutes
-- |
-- | We don't try to be precise, e.g. users with Online availability are ignored
persistLastSeen ∷ WebSocketReaderLite → Effect Unit
persistLastSeen reading@{ userAvailability } = do
      availabilities ← ER.read userAvailability
      when (not $ DH.isEmpty availabilities) do
            let run = R.runBaseAff' <<< RE.catch (const (pure unit)) <<< RR.runReader reading <<< SID.upsertLastSeen <<< writeJSON <<< DA.catMaybes $ DH.toArrayBy lastSeens availabilities
            EA.launchAff_ $ EA.catchError run logError
      where
      lastSeens id = case _ of
            { availability: LastSeen (DateTimeWrapper date)} → Just { who: id, date: DT date }
            _ → Nothing

      logError = liftEffect <<< EC.logShow
