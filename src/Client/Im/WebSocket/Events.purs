module Client.Im.WebSocket.Events (startWebSocket, toggleConnectedWebSocket, sendPing, receiveMessage) where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCNT
import Client.Im.Chat as CIC
import Client.Im.Contacts as CICN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Notification as CIUC
import Client.Im.Scroll as CISM
import Client.Im.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array ((:))
import Data.Array as DA
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable as DT
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Now as EN
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer (IntervalId, TimeoutId)
import Effect.Timer as ET
import Flame as F
import Flame.Subscription as FS
import Foreign as FO
import Safe.Coerce as SC
import Server.WebSocket (WebSocketConnection)
import Shared.Availability (Availability(..))
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Experiments.Types as SET
import Shared.Im.Contact as SIC
import Shared.Im.Types (ClientMessagePayload, Contact, EditedMessagePayload, FullWebSocketPayloadClient(..), HistoryMessage, ImMessage(..), ImModel, MessageStatus(..), RetryableRequest(..), TimeoutIdWrapper(..), WebSocketPayloadClient(..), WebSocketPayloadServer(..), DeletedMessagePayload)
import Shared.Json as SJ
import Shared.Options.MountPoint (experimentsId, imId, profileId)
import Shared.Privilege (Privilege)
import Shared.Profile.Types as SPT
import Shared.ResponseError (DatabaseError(..))
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.Socket.Event.EventTypes (onClose, onError, onMessage, onOpen)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

-- | Reconection, ping and privilege update are done with setInterval
type WebSocketState =
      { webSocket ∷ WebSocket
      , reconnectId ∷ Maybe TimeoutId
      , pingId ∷ Maybe IntervalId
      , lastPongDate ∷ DateTime
      , privilegesId ∷ Maybe IntervalId
      }

-- | Web socket state is kept in a ref since the connection can be closed and recreated by events any time
startWebSocket ∷ Effect (Ref WebSocketState)
startWebSocket = do
      webSocket ← CIW.createWebSocket
      now ← EN.nowDateTime
      webSocketStateRef ← ER.new { webSocket, reconnectId: Nothing, pingId: Nothing, privilegesId: Nothing, lastPongDate: now }

      setUpWebsocket webSocketStateRef
      pure webSocketStateRef

-- | Set listeners for web socket events
setUpWebsocket ∷ Ref WebSocketState → Effect Unit
setUpWebsocket webSocketStateRef = do
      state ← ER.read webSocketStateRef
      let webSocketTarget = WSW.toEventTarget state.webSocket
      openListener ← WET.eventListener (handleOpen webSocketStateRef)
      messageListener ← WET.eventListener (handleMessage webSocketStateRef)
      closeListener ← WET.eventListener (handleClose webSocketStateRef)
      errorListener ← WET.eventListener (handleClose webSocketStateRef)

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget
      WET.addEventListener onError errorListener false webSocketTarget

handleOpen ∷ Ref WebSocketState → Event → Effect Unit
handleOpen webSocketStateRef _ = do
      state ← ER.read webSocketStateRef
      --when the connection is open and the user is on the page serialize their availability
      sendSetOnline state.webSocket
      --close event may have set up to open a new connection after this timeout
      case state.reconnectId of
            Nothing → pure unit
            Just id → do
                  ET.clearTimeout id
                  FS.send imId <<< SpecialRequest <<< CheckMissedEvents <<< Just $ DateTimeWrapper state.lastPongDate
      newPrivilegesId ← ET.setInterval privilegeDelay (pollPrivileges state.webSocket)
      newPingId ← ET.setInterval pingDelay ping
      ER.modify_ (_ { pingId = Just newPingId, privilegesId = Just newPrivilegesId, reconnectId = Nothing }) webSocketStateRef
      FS.send imId $ ToggleConnected true
      --check if the page needs to be reloaded
      CIW.sendPayload state.webSocket UpdateHash

      where
      privilegeDelay = 1000 * 60 * 60
      pollPrivileges webSocket = CIW.sendPayload webSocket UpdatePrivileges

      pingDelay = 1000 * 30
      ping = do
            isFocused ← CCD.documentHasFocus
            FS.send imId $ SendPing isFocused

      sendSetOnline webSocket = do
            isFocused ← CCD.documentHasFocus
            when isFocused $ CIW.sendPayload webSocket SetOnline

-- | Handle an incoming (json encoded) message from the server
handleMessage ∷ Ref WebSocketState → Event → Effect Unit
handleMessage webSocketStateRef event = do
      let payload = SU.fromRight <<< CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.fromJust $ WSEM.fromEvent event
      let message = SU.fromRight $ SJ.fromJson payload
      case message of
            CloseConnection cc → FS.send imId $ Logout cc --user has been banned or server is on fire
            Pong p → do
                  now ← EN.nowDateTime
                  ER.modify_ (_ { lastPongDate = now }) webSocketStateRef
                  FS.send imId $ DisplayAvailability p.status --pings are set up when the socket is open
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
            --random so the server is not flooded with a zillion simultaneous connections
            milliseconds ← ERD.randomInt 2000 10000
            id ← ET.setTimeout milliseconds do
                  webSocket ← CIW.createWebSocket
                  ER.modify_ (_ { webSocket = webSocket }) webSocketStateRef
                  setUpWebsocket webSocketStateRef
            ER.modify_ (_ { reconnectId = Just id }) webSocketStateRef

-- | Send ping with users to learn availability of
sendPing ∷ WebSocket → Boolean → ImModel → NoMessages
sendPing webSocket isActive model =
      model /\
            [ liftEffect do
                    CIW.sendPayload webSocket $ Ping
                          { isActive
                          , statusFor: DA.nub (map _.id model.suggestions <> map (_.id <<< _.user) (DA.filter ((_ /= Unavailable) <<< _.availability <<< _.user) model.contacts)) -- user might be both in contacts and suggestions
                          }
                    pure Nothing
            ]

-- | Handle content messages from the server
receiveMessage ∷ WebSocket → Boolean → WebSocketPayloadClient → ImModel → MoreMessages
receiveMessage webSocket isFocused payload model = case payload of
      ServerChangedStatus cs → receiveStatusChange cs model
      ServerReceivedMessage rm → receiveAcknowledgement rm model
      NewIncomingMessage ni → receiveIncomingMessage webSocket isFocused ni model
      NewEditedMessage ni → receiveEditedMessage webSocket isFocused ni model
      NewDeletedMessage nd → receiveDeletedMessage nd model
      ContactTyping tp → receiveTyping tp model
      CurrentPrivileges kp → receivePrivileges kp model
      CurrentHash newHash → receiveHash newHash model
      ContactUnavailable cu → receiveUnavailable cu model
      BadMessage bm → receiveBadMessage bm model
      PayloadError p → receivePayloadError p model

-- | Update message status
receiveStatusChange ∷ { ids ∷ Array Int, status ∷ MessageStatus, userId ∷ Int } → ImModel → NoMessages
receiveStatusChange received model =
      model
            { contacts = updatedContacts
            } /\ [ when (received.status == Read) (liftEffect $ CIUC.updateTabCount model.user.id updatedContacts) *> pure Nothing ]
      where
      updatedContacts = updateHistory model.contacts received.userId updateStatus

      updateStatus history
            | DA.elem history.id received.ids = history { status = received.status }
            | otherwise = history

-- | Move status from 'sending' to 'sent' and update message id
receiveAcknowledgement ∷ { id ∷ Int, previousId ∷ Int, userId ∷ Int } → ImModel → NoMessages
receiveAcknowledgement received model = F.noMessages model
      { contacts = updateHistory model.contacts received.userId updateIdStatus
      }
      where
      updateIdStatus history
            | history.id == received.previousId = history { id = received.id, status = Received }
            | otherwise = history

-- | Remove a message from contact's history
receiveDeletedMessage ∷ DeletedMessagePayload → ImModel → NoMessages
receiveDeletedMessage deleted model = F.noMessages model
      { contacts = rm <$> model.contacts
      }
      where
      rm contact
            | contact.user.id == deleted.userId = contact { history = DA.filter ((deleted.id /= _) <<< _.id) contact.history }
            | otherwise = contact

-- | A new message from others users or sent by the logged user from another connection
receiveIncomingMessage ∷ WebSocket → Boolean → ClientMessagePayload → ImModel → NextMessage
receiveIncomingMessage webSocket isFocused payload model =
      if DA.elem userId model.blockedUsers then
            F.noMessages model --prevents messages already in flight to deliver after block
      else
            case fromIncomingMessage payload unsuggestedModel of
                  Left id →
                        --new message from an user that is not currently in the contacts
                        unsuggestedModel /\ [ CCNT.retryableResponse (CheckMissedEvents Nothing) DisplayNewContacts $ request.im.contact { query: { id } } ]
                  Right updatedModel | model.user.id == payload.senderId →
                        --syncing message sent from other connections
                        updatedModel /\ [ liftEffect (CIUC.updateTabCount model.user.id updatedModel.contacts) *> pure Nothing ]
                  Right
                        updatedModel | isFocused && ((_.id <<< _.user) <$> SIC.maybeFindContact updatedModel.chatting updatedModel.contacts) == Just userId →
                        --new message from the user being chatted with
                        CICN.setMessageStatus webSocket userId Read updatedModel # withExtraMessage CISM.scrollLastMessageAff
                  Right updatedModel →
                        --new message when away/other usesr
                        CICN.setMessageStatus webSocket userId Delivered updatedModel # withExtraMessage (CIUC.notify' updatedModel [ userId ])
      where
      unsuggestedModel = unsuggest payload.recipientId model

      userId
            | payload.recipientId == model.user.id = payload.senderId
            | otherwise = payload.recipientId

      withExtraMessage e (m /\ ms) = m /\ e : ms

receiveEditedMessage ∷ WebSocket → Boolean → EditedMessagePayload → ImModel → NextMessage
receiveEditedMessage webSocket isFocused payload model =
      if DA.elem userId model.blockedUsers then
            F.noMessages model
      else if model.user.id == payload.senderId then
            updatedModel /\ [ liftEffect (CIUC.updateTabCount model.user.id updatedModel.contacts) *> pure Nothing ]
      else
            CICN.setMessageStatus webSocket userId (if isFocused && model.chatting == Just userId then Read else Delivered) updatedModel
      where
      updateContent history
            | history.id == payload.id = history { content = payload.content, edited = true }
            | otherwise = history

      updatedModel = model { contacts = updateHistory model.contacts userId updateContent }

      userId
            | payload.recipientId == model.user.id = payload.senderId
            | otherwise = payload.recipientId

-- | Set typing status and a timeout to clear it
receiveTyping ∷ { id ∷ Int } → ImModel → MoreMessages
receiveTyping received model = CIC.toggleTyping received.id true model /\
      [ liftEffect do
              DT.traverse_ (ET.clearTimeout <<< SC.coerce) model.typingIds
              newId ← ET.setTimeout 1000 <<< FS.send imId $ NoTyping received.id
              pure <<< Just $ TypingId newId
      ]

-- | User privileges are requested on socket (re)connection
receivePrivileges ∷ { karma ∷ Int, privileges ∷ Array Privilege } → ImModel → NoMessages
receivePrivileges received model =
      model
            { user
                    { karma = received.karma
                    , privileges = received.privileges
                    }
            } /\
            [ do
                    liftEffect do
                          FS.send profileId $ SPT.UpdatePrivileges received
                          FS.send experimentsId $ SET.UpdatePrivileges received
                    pure Nothing
            ]

-- | When the site updates the bundle file hashes change
receiveHash ∷ String → ImModel → NoMessages
receiveHash newHash model = F.noMessages model
      { imUpdated = newHash /= model.hash
      }

receiveUnavailable ∷ _ → ImModel → NoMessages
receiveUnavailable received model =
      F.noMessages $ unsuggest received.userId model
            { contacts = case received.temporaryMessageId of
                    Nothing → updatedContacts
                    Just id → markErroredMessage updatedContacts received.userId id
            }
      where
      updatedContacts = setContactUnavailable model.contacts received.userId

-- | Message sent was unsanitary
receiveBadMessage ∷ _ → ImModel → NoMessages
receiveBadMessage received model = F.noMessages model
      { contacts = case received.temporaryMessageId of
              Nothing → model.contacts
              Just id → markErroredMessage model.contacts received.userId id
      }

receivePayloadError ∷ { origin ∷ WebSocketPayloadServer, context ∷ Maybe DatabaseError } → ImModel → NoMessages
receivePayloadError received model = case received.origin of
      OutgoingMessage msg → F.noMessages model
            { contacts =
                    --assume that it is because the other user no longer exists
                    if received.context == Just MissingForeignKey then
                          setContactUnavailable model.contacts msg.userId
                    else
                          markErroredMessage model.contacts msg.userId msg.id
            }
      _ → F.noMessages model

-- | Block, ban or account termination
setContactUnavailable ∷ Array Contact → Int → Array Contact
setContactUnavailable contacts userId = updateContact <$> contacts
      where
      updateContact contact
            | contact.user.id == userId = contact
                    { user
                            { availability = Unavailable
                            }
                    }
            | otherwise = contact

markErroredMessage ∷ Array Contact → Int → Int → Array Contact
markErroredMessage contacts userId messageId = updateHistory contacts userId updateStatus
      where
      updateStatus history
            | messageId == history.id = history { status = Errored }
            | otherwise = history

-- | Remove an user from the suggestions
unsuggest ∷ Int → ImModel → ImModel
unsuggest userId model = model
      { suggestions = updatedSuggestions
      , suggesting = updatedSuggesting
      }
      where
      unsuggestedIndex = DA.findIndex ((userId == _) <<< _.id) model.suggestions
      updatedSuggestions = DM.fromMaybe model.suggestions do
            i ← unsuggestedIndex
            DA.deleteAt i model.suggestions
      updatedSuggesting
            | unsuggestedIndex /= Nothing && unsuggestedIndex < Just model.suggesting = max 0 (model.suggesting - 1)
            | otherwise = model.suggesting

-- | Updated contacts if user is already there
fromIncomingMessage ∷ ClientMessagePayload → ImModel → Either Int ImModel
fromIncomingMessage payload model =
      if DA.any ((userId == _) <<< _.id <<< _.user) model.contacts then
            Right model
                  { contacts = map updateContact model.contacts
                  }
      else
            Left userId
      where
      userId
            | payload.senderId == model.user.id = payload.recipientId
            | otherwise = payload.senderId

      updateContact contact
            | contact.user.id == userId = contact
                    { lastMessageDate = payload.date
                    , history = DA.snoc contact.history $
                            { status: Received
                            , sender: payload.senderId
                            , recipient: payload.recipientId
                            , id: payload.id
                            , edited: false
                            , content: payload.content
                            , date: payload.date
                            }
                    }
            | otherwise = contact

updateHistory ∷ Array Contact → Int → (HistoryMessage → HistoryMessage) → Array Contact
updateHistory contacts userId updater = updateIt <$> contacts
      where
      updateIt contact
            | contact.user.id == userId = contact
                    { history = updater <$> contact.history
                    }
            | otherwise = contact

toggleConnectedWebSocket ∷ Boolean → ImModel → MoreMessages
toggleConnectedWebSocket isConnected model =
      model
            { hasTriedToConnectYet = true
            , isWebSocketConnected = isConnected
            , errorMessage = if not isConnected then lostConnectionMessage else if model.errorMessage == lostConnectionMessage then "" else model.errorMessage
            } /\
            if model.hasTriedToConnectYet && isConnected then
                  [ liftEffect $ FS.send imId CheckUserExpiration *> pure Nothing
                  ]
            else
                  [ pure $ Just SetDeliveredStatus ] -- when the site is first loaded, signal that messages have been delivered
      where
      lostConnectionMessage =
            "Reconnecting..."
