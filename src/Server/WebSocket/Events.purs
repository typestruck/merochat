module Server.WebSocket.Events where

import Prelude

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
import Data.Set (Set)
import Data.Set as DS
import Data.Time.Duration (Minutes)
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (class MonadEffect)
import Effect.Class as EC
import Effect.Console as ECS
import Effect.Exception (Error, throw)
import Effect.Exception.Unsafe as EEU
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
import Server.Database.KarmaLeaderboard as SIKL
import Server.Database.LastSeen (LastSeen)
import Server.Database.Privileges as SIP
import Server.Database.Users as SDU
import Server.Effect (BaseEffect, BaseReader, Configuration)
import Server.Effect as SE
import Server.Im.Action as SIA
import Server.Im.Database.Execute as SIDE
import Server.Push (PushMessage(..))
import Server.Push as SP
import Server.Settings.Action as SSA
import Server.Token as ST
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.Availability (Availability(..))
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SDT
import Shared.Im.Types (AfterLogout(..), DeletedRecord, EditedRecord, FullWebSocketPayloadClient(..), MessageError(..), MessageStatus(..), OutgoingRecord, WebSocketPayloadClient(..), WebSocketPayloadServer(..))
import Shared.Json as SJ
import Shared.Resource (updateHash)
import Shared.ResponseError (DatabaseError, ResponseError(..))
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Simple.JSON (class WriteForeign)
import Simple.JSON as SJS

-- | Keep each users web socket/availability in a ref
type UserAvailability =
      { connections ∷ HashMap String WebSocketConnection
      , trackedBy ∷ Set Int
      , hasPwa ∷ Boolean --only send push notifications if user registered for it
      , availability ∷ Availability
      }

type WebSocketEffect = BaseEffect WebSocketReader Unit

type WebSocketReader = BaseReader
      ( loggedUserId ∷ Int
      , token ∷ String --use this to tell connections apart
      , configuration ∷ Configuration
      , allUsersAvailabilityRef ∷ Ref (HashMap Int UserAvailability)
      )

type WebSocketReaderLite = BaseReader
      ( allUsersAvailabilityRef ∷ Ref (HashMap Int UserAvailability)
      )

-- | Wrapper so we can serialize dates in a way postgresql understands
newtype DT = DT DateTime

-- | How often do we check for inactive connections
inactiveInterval ∷ Int
inactiveInterval = 1000 * 60 * inactiveMinutes

inactiveMinutes ∷ Int
inactiveMinutes = 30

handleConnection ∷ Configuration → Pool → Ref (HashMap Int UserAvailability) → WebSocketConnection → Request → Effect Unit
handleConnection configuration pool allUsersAvailabilityRef connection request = EA.launchAff_ do
      userId ← SE.poolEffect pool Nothing $ ST.userIdFromToken configuration.tokenSecret token
      case userId of
            Nothing → EC.liftEffect do
                  --this can be made more clear for the end user
                  sendWebSocketMessage connection $ CloseConnection LoginPage
                  ECS.log "terminated due to auth error"
            Just loggedUserId → do
                  pwa ← SE.poolEffect pool false $ SDU.hasPwa loggedUserId
                  EC.liftEffect do
                        now ← EN.nowDateTime
                        ER.modify_ (DH.alter (upsertUserAvailability now pwa) loggedUserId) allUsersAvailabilityRef
                        SW.onError connection handleError
                        SW.onClose connection (handleClose token loggedUserId pool allUsersAvailabilityRef)
                        SW.onMessage connection (runMessageHandler loggedUserId)
      where
      token = DM.fromMaybe "" do
            uncooked ← FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked

      initialAvailability hasPwa = { connections: DH.fromArray [ Tuple token connection ], trackedBy: DS.empty, hasPwa, availability: None }
      upsertUserAvailability date hasPwa =
            case _ of
                  Nothing → Just $ makeUserAvailabity (initialAvailability hasPwa) (Right token) date None --could also query the db
                  Just userAvailability → Just $ makeUserAvailabity (userAvailability { hasPwa = hasPwa, connections = DH.insert token connection userAvailability.connections }) (Right token) date None

      runMessageHandler loggedUserId (WebSocketMessage message) = do
            case SJ.fromJson message of
                  Right payload → do
                        let
                              run =
                                    R.runBaseAff'
                                          <<< RE.catch (\e → reportError payload (checkInternalError e) e)
                                          <<<
                                                RR.runReader { allUsersAvailabilityRef, configuration, pool, loggedUserId, token } $
                                          handleMessage payload
                        EA.launchAff_ <<< EA.catchError run $ reportError payload Nothing
                  Left error → do
                        SW.terminate connection
                        ECS.log $ "terminated due to serialization error: " <> error

      reportError ∷ ∀ a b. MonadEffect b ⇒ WebSocketPayloadServer → Maybe DatabaseError → a → b Unit
      reportError origin context _ = sendWebSocketMessage connection <<< Content $ PayloadError { origin, context }

      checkInternalError = case _ of
            InternalError { context } → context
            _ → Nothing

handleError ∷ Error → Effect Unit
handleError = ECS.log <<< show

handleClose ∷ String → Int → Pool -> Ref (HashMap Int UserAvailability) → CloseCode → CloseReason → Effect Unit
handleClose token loggedUserId pool allUsersAvailabilityRef _ _ = do
      allUsersAvailability ← ER.read allUsersAvailabilityRef
      now ← EN.nowDateTime
      let userAvailability = SU.fromJust $ DH.lookup loggedUserId allUsersAvailability
      --if thats the only open connection for an online user we need to flag it as not online
      if userAvailability.availability == Online && DH.size userAvailability.connections == 1 then do
            let lastSeen = LastSeen $ DateTimeWrapper now
            updatedAllUsersAvailability ← ER.modify (DH.update (removeConnection now lastSeen) loggedUserId) allUsersAvailabilityRef
            let updatedUserAvaibility = SU.fromJust $ DH.lookup loggedUserId updatedAllUsersAvailability
            DF.traverse_ (sendTrackedAvailability updatedAllUsersAvailability lastSeen loggedUserId) updatedUserAvaibility.trackedBy
            EA.launchAff_ <<< SE.poolEffect pool unit $ SIDE.upsertLastSeen loggedUserId now
      else
            ER.modify_ (DH.update (removeConnection now None) loggedUserId) allUsersAvailabilityRef
      where
      removeConnection now availability userAvailability = Just $ makeUserAvailabity userAvailability (Left token) now availability

handleMessage ∷ WebSocketPayloadServer → WebSocketEffect
handleMessage payload = do
      context ← RR.ask
      allUsersAvailability ← EC.liftEffect $ ER.read context.allUsersAvailabilityRef
      case payload of
            Ping → sendPong context.token context.loggedUserId context.allUsersAvailabilityRef
            OutgoingMessage message → sendOutgoingMessage context.token context.loggedUserId allUsersAvailability message
            EditedMessage message → sendEditedMessage context.token context.loggedUserId allUsersAvailability message
            TrackAvailabilityFor for → trackAvailability context.loggedUserId context.allUsersAvailabilityRef for
            DeletedMessage message → unsendMessage context.token context.loggedUserId allUsersAvailability message
            ChangeStatus changes → sendStatusChange context.token context.loggedUserId allUsersAvailability changes
            Typing { id } → sendTyping context.loggedUserId allUsersAvailability id
            UpdateAvailability flags → updateAvailability context.token context.loggedUserId context.allUsersAvailabilityRef flags
            UpdatePrivileges → sendUpdatedPrivileges context.loggedUserId allUsersAvailability
            UpdateHash → sendUpdatedHash context.loggedUserId allUsersAvailability
            UnavailableFor { id } → sendUnavailability context.loggedUserId allUsersAvailability id
            Ban { id } → sendBan allUsersAvailability id

updateAvailability ∷ String → Int → Ref (HashMap Int UserAvailability) → { online ∷ Boolean } → WebSocketEffect
updateAvailability token loggedUserId allUsersAvailabilityRef flags = do
      now ← zeroFromMinutes <$> EC.liftEffect EN.nowDateTime
      allUsersAvailability ← EC.liftEffect $ ER.read allUsersAvailabilityRef
      let userAvailability = SU.fromJust $ DH.lookup loggedUserId allUsersAvailability
      let
            availability
                  | flags.online = Online
                  | otherwise = LastSeen $ DateTimeWrapper now
      when (shouldUpdate availability userAvailability.availability) do
            EC.liftEffect $ ER.modify_ (DH.insert loggedUserId (makeUserAvailabity userAvailability (Right token) now availability)) allUsersAvailabilityRef
            DF.traverse_ (sendAvailability allUsersAvailability availability) userAvailability.trackedBy
            SIDE.upsertLastSeen loggedUserId now
      where
      shouldUpdate new old = case new, old of
            LastSeen (DateTimeWrapper dt), LastSeen (DateTimeWrapper anotherDt) → dt > anotherDt
            newAvailability, oldAvailability → newAvailability /= oldAvailability

      --ignore last seen if only difference is in seconds / milliseconds
      zeroFromMinutes dt = DDT.modifyTime (DDT.setSecond (SU.fromJust $ DEN.toEnum 0) <<< DDT.setMillisecond (SU.fromJust $ DEN.toEnum 0)) dt

      sendAvailability allUsersAvailability availability userId = case DH.lookup userId allUsersAvailability of
            Just found → DF.traverse_ (\connection → sendWebSocketMessage connection <<< Content $ TrackedAvailability { id: loggedUserId, availability }) found.connections
            Nothing → pure unit

trackAvailability ∷ Int → Ref (HashMap Int UserAvailability) → { ids ∷ Array Int } → WebSocketEffect
trackAvailability loggedUserId allUsersAvailabilityRef for = R.liftEffect $ DF.traverse_ (\id → ER.modify_ (DH.update track id) allUsersAvailabilityRef) for.ids
      where
      track userAvailability = Just userAvailability { trackedBy = DS.insert loggedUserId userAvailability.trackedBy }

sendBan ∷ HashMap Int UserAvailability → Int → WebSocketEffect
sendBan allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      SSA.changePrivacySettings userId
            { messageTimestamps: true
            , onlineStatus: true
            , readReceipts: true
            , typingStatus: true
            , profileVisibility: TemporarilyBanned
            }
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection $ CloseConnection Banned

sendUpdatedPrivileges ∷ Int → HashMap Int UserAvailability → WebSocketEffect
sendUpdatedPrivileges loggedUserId allUsersAvailability = do
      karma ← SIKL.fetchKarma loggedUserId
      privileges ← SIP.fetchPrivileges karma
      let userAvailability = DH.lookup loggedUserId allUsersAvailability
      withConnections userAvailability (send { karma, privileges })
      where
      send payload connection = sendWebSocketMessage connection <<< Content $ CurrentPrivileges payload

sendUpdatedHash ∷ Int → HashMap Int UserAvailability → WebSocketEffect
sendUpdatedHash loggedUserId allUsersAvailability = do
      let userAvailability = DH.lookup loggedUserId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ CurrentHash updateHash

sendTyping ∷ Int → HashMap Int UserAvailability → Int → WebSocketEffect
sendTyping loggedUserId allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ ContactTyping { id: loggedUserId }

sendPong ∷ String → Int → Ref (HashMap Int UserAvailability) → WebSocketEffect
sendPong token loggedUserId allUsersAvailabilityRef = do
      date ← R.liftEffect EN.nowDateTime
      allUsersAvailability ← R.liftEffect $ ER.read allUsersAvailabilityRef
      R.liftEffect $ ER.modify_ (DH.update (setLastPing date) loggedUserId) allUsersAvailabilityRef
      let userAvailability = SU.fromJust $ DH.lookup loggedUserId allUsersAvailability
      sendWebSocketMessage (SU.fromJust $ DH.lookup token userAvailability.connections) Pong
      where
      setLastPing date userAvailability = Just userAvailability { connections = DH.update (Just <<< SW.lastPing date) token userAvailability.connections }

sendUnavailability ∷ Int → HashMap Int UserAvailability → Int → WebSocketEffect
sendUnavailability loggedUserId allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ ContactUnavailable
            { userId: loggedUserId
            , temporaryMessageId: Nothing
            }

sendStatusChange ∷ String → Int → HashMap Int UserAvailability → { ids ∷ Array (Tuple Int (Array Int)), status ∷ MessageStatus } → WebSocketEffect
sendStatusChange token loggedUserId allUsersAvailability changes = do
      SIDE.changeStatus loggedUserId changes.status $ DA.concatMap DT.snd changes.ids

      DF.traverse_ sendReceipients changes.ids

      let loggedUserAvailability = SU.fromJust $ DH.lookup loggedUserId allUsersAvailability
      let loggedUserConnections = DH.values $ DH.filterKeys (token /= _) loggedUserAvailability.connections
      DF.traverse_ (sendLoggedUser loggedUserConnections) changes.ids

      when (changes.status == Read && loggedUserAvailability.hasPwa) $ DF.traverse_ sendPushStatus changes.ids
      where
      sendPushStatus (userId /\ _) = EC.liftEffect <<< SP.push loggedUserId "" $ MessageReadSomewhereElse { userId }

      sendReceipients (userId /\ messageIds) = do
            let userAvailability = DH.lookup userId allUsersAvailability
            withConnections userAvailability $ \connection → send connection messageIds loggedUserId
      --other connections opened by this user need also to be alerted
      sendLoggedUser loggedUserConnections (userId /\ messageIds) = DF.traverse_ (\connection → send connection messageIds userId) loggedUserConnections

      send connection messageIds userId =
            sendWebSocketMessage connection <<< Content $ ServerChangedStatus
                  { ids: messageIds
                  , status: changes.status
                  , userId
                  }

-- | Send a message or another user or sync a message sent from another connection
sendOutgoingMessage ∷ String → Int → HashMap Int UserAvailability → OutgoingRecord → WebSocketEffect
sendOutgoingMessage token loggedUserId allUsersAvailability outgoing = do
      processed ← SIA.processMessage loggedUserId outgoing.userId outgoing.content
      case processed of
            Right (messageId /\ content) → do
                  now ← R.liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let receipientUserAvailability = DH.lookup outgoing.userId allUsersAvailability
                  withConnections receipientUserAvailability (sendRecipient messageId content now)

                  let loggedUserConnections = (SU.fromJust loggedUserAvailability).connections
                  let senderConnection = DH.values $ DH.filterKeys (token == _) loggedUserConnections
                  DF.traverse_ (acknowledgeMessage messageId) senderConnection

                  let otherConnections = DH.values $ DH.filterKeys (token /= _) loggedUserConnections
                  DF.traverse_ (sendRecipient messageId content now) otherConnections

                  when (DM.maybe false _.hasPwa receipientUserAvailability) <<< R.liftEffect <<< SP.push outgoing.userId outgoing.userName $ IncomingMessage
                        { id: messageId
                        , senderId: loggedUserId
                        , recipientId: outgoing.userId
                        , content
                        , date: now
                        }

                  DM.maybe (pure unit) (SIA.processKarma loggedUserId outgoing.userId) outgoing.turn
            Left UserUnavailable →
                  sendLoggedUser <<< Content $ ContactUnavailable
                        { userId: outgoing.userId
                        , temporaryMessageId: Just outgoing.id
                        }
            Left InvalidMessage →
                  sendLoggedUser <<< Content $ BadMessage
                        { userId: outgoing.userId
                        , temporaryMessageId: Just outgoing.id
                        }
      where
      sendRecipient messageId content date connection =
            sendWebSocketMessage connection <<< Content $ NewIncomingMessage
                  { id: messageId
                  , senderId: loggedUserId
                  , recipientId: outgoing.userId
                  , content
                  , date
                  }

      loggedUserAvailability = DH.lookup loggedUserId allUsersAvailability

      acknowledgeMessage messageId connection = sendWebSocketMessage connection <<< Content $ ServerReceivedMessage
            { previousId: outgoing.id
            , id: messageId
            , userId: outgoing.userId
            }

      sendLoggedUser payload = do
            withConnections loggedUserAvailability $ \connection → sendWebSocketMessage connection payload

sendEditedMessage ∷ String → Int → HashMap Int UserAvailability → EditedRecord → WebSocketEffect
sendEditedMessage token loggedUserId allUsersAvailability edited = do
      processed ← SIA.editMessage loggedUserId edited.userId edited.id edited.content
      case processed of
            Right content → do
                  let receipientUserAvailability = DH.lookup edited.userId allUsersAvailability
                  withConnections receipientUserAvailability (sendRecipient content)

                  let loggedUserConnections = (SU.fromJust loggedUserAvailability).connections
                  let senderConnection = DH.values $ DH.filterKeys (token == _) loggedUserConnections
                  DF.traverse_ acknowledgeMessage senderConnection

                  let otherConnections = DH.values $ DH.filterKeys (token /= _) loggedUserConnections
                  DF.traverse_ (sendRecipient content) otherConnections
            Left UserUnavailable →
                  sendLoggedUser <<< Content $ ContactUnavailable
                        { userId: edited.userId
                        , temporaryMessageId: Just edited.id
                        }
            Left InvalidMessage →
                  sendLoggedUser <<< Content $ BadMessage
                        { userId: edited.userId
                        , temporaryMessageId: Just edited.id
                        }
      where
      sendRecipient content connection =
            sendWebSocketMessage connection <<< Content $ NewEditedMessage
                  { id: edited.id
                  , senderId: loggedUserId
                  , recipientId: edited.userId
                  , content
                  }

      loggedUserAvailability = DH.lookup loggedUserId allUsersAvailability

      acknowledgeMessage connection = sendWebSocketMessage connection <<< Content $ ServerReceivedMessage
            { previousId: edited.id
            , id: edited.id
            , userId: edited.userId
            }

      sendLoggedUser payload = do
            withConnections loggedUserAvailability $ \connection → sendWebSocketMessage connection payload

unsendMessage ∷ String → Int → HashMap Int UserAvailability → DeletedRecord → WebSocketEffect
unsendMessage token loggedUserId allUsersAvailability deleted = do
      SIA.unsendMessage loggedUserId deleted.userId deleted.id

      let receipientUserAvailability = DH.lookup deleted.userId allUsersAvailability
      withConnections receipientUserAvailability (send loggedUserId)

      let loggedUserConnections = (SU.fromJust loggedUserAvailability).connections
      let senderConnection = DH.values $ DH.filterKeys (token == _) loggedUserConnections
      DF.traverse_ (send deleted.userId) senderConnection

      let otherConnections = DH.values $ DH.filterKeys (token /= _) loggedUserConnections
      DF.traverse_ (send deleted.userId) otherConnections
      where
      send userId connection =
            sendWebSocketMessage connection <<< Content $ NewDeletedMessage
                  { id: deleted.id
                  , userId
                  }

      loggedUserAvailability = DH.lookup loggedUserId allUsersAvailability

makeUserAvailabity ∷ UserAvailability → Either String String → DateTime → Availability → UserAvailability
makeUserAvailabity old token date currentAvailability =
      { hasPwa: old.hasPwa
      , trackedBy: old.trackedBy
      , connections:
              case token of
                    Right t → DH.update (Just <<< SW.lastPing date) t old.connections --ping on the socket is used to determine inactive connections
                    Left t → DH.delete t old.connections
      , availability:
              case currentAvailability of
                    Online → Online
                    None → old.availability
                    c → c
      }

-- | Send a json encoded message
sendWebSocketMessage ∷ ∀ b. MonadEffect b ⇒ WebSocketConnection → FullWebSocketPayloadClient → b Unit
sendWebSocketMessage connection = EC.liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson

-- | Every `inactiveMinutes` check for dead connections
terminateInactive ∷ Pool -> Ref (HashMap Int UserAvailability) → Effect Unit
terminateInactive pool allUsersAvailabilityRef = do
      removeInactiveConnections allUsersAvailabilityRef
      trackAvailabilityFromTerminated pool allUsersAvailabilityRef

removeInactiveConnections ∷ Ref (HashMap Int UserAvailability) → Effect Unit
removeInactiveConnections allUsersAvailabilityRef = do
      now ← EN.nowDateTime
      allUsersAvailability ← ER.read allUsersAvailabilityRef
      DF.traverse_ (remove now) $ DH.toArrayBy (/\) allUsersAvailability
      where
      remove now (userId /\ userAvailability) = do
            let inactiveConnections = DH.filter (isInactive now <<< SW.getLastPing) userAvailability.connections
            unless (DH.isEmpty inactiveConnections) do
                  DF.traverse_ SW.terminate $ DH.values inactiveConnections
                  ER.modify_ (DH.update (updateConnections now inactiveConnections) userId) allUsersAvailabilityRef

      --if the connection has not pinged in 2 minutes it is likely dead
      inactiveMinutesHere = 2
      isInactive now lastPing = inactiveMinutesHere <= DI.floor (DN.unwrap (DDT.diff now lastPing ∷ Minutes))
      updateConnections now connections userAvailability =
            let
                  liveConnections = DH.difference userAvailability.connections connections
            in
                  Just userAvailability
                        { connections = liveConnections
                        , availability =
                                if userAvailability.availability == Online && DH.isEmpty liveConnections then
                                      LastSeen <<< DateTimeWrapper <<< DM.fromMaybe now <<< DF.maximum <<< map SW.getLastPing $ DH.values userAvailability.connections
                                else
                                      userAvailability.availability
                        }

--if all user connections were inactive then availibity is last seen and thus needs to be communicated
trackAvailabilityFromTerminated ∷ Pool -> Ref (HashMap Int UserAvailability) → Effect Unit
trackAvailabilityFromTerminated pool allUsersAvailabilityRef = do
      allUsersAvailability ← ER.read allUsersAvailabilityRef
      let terminatedConnections = DA.filter (DH.isEmpty <<< _.connections <<< DT.snd) $ DH.toArrayBy (/\) allUsersAvailability
      unless (DA.null terminatedConnections) do
            DF.traverse_ (sendAvailability allUsersAvailability) terminatedConnections
            EA.launchAff_ <<< SE.poolEffect pool unit <<< SIDE.bulkUpsertLastSeen <<< SJS.writeJSON $ map whoDate terminatedConnections
      where
      sendAvailability allUsersAvailability (userId /\ userAvailability) = DF.traverse_ (sendTrackedAvailability allUsersAvailability userAvailability.availability userId) userAvailability.trackedBy

      whoDate (userId /\ userAvailability) = case userAvailability.availability of
            LastSeen (DateTimeWrapper dt) -> { who : userId, date : DT dt}
            a -> EEU.unsafeThrow ("Invalid availability for trackAvailabilityFromTerminated: " <> show a)

sendTrackedAvailability ∷ HashMap Int UserAvailability → Availability → Int → Int → Effect Unit
sendTrackedAvailability allUsersAvailability availability trackedUserId userId = case DH.lookup userId allUsersAvailability of
      Just userAvailability → DF.traverse_ (\connection → sendWebSocketMessage connection <<< Content $ TrackedAvailability { id: trackedUserId, availability }) userAvailability.connections
      Nothing → pure unit

withConnections ∷ Maybe UserAvailability → (WebSocketConnection → WebSocketEffect) → WebSocketEffect
withConnections userAvailability handler =
      case userAvailability of
            Just ua → DF.traverse_ handler $ DH.values ua.connections
            Nothing → pure unit

instance Newtype DT DateTime
instance WriteForeign DT where
      writeImpl (DT (DateTime dt (Time h m s ms))) = F.unsafeToForeign (SDT.formatIsoDate' dt <> "t" <> time <> "+0000")
            where
            time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> "." <> show (DEN.fromEnum ms)
