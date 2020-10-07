module Client.IM.Main where

import Prelude
import Shared.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Suggestion as CIS
import Client.IM.Unread as CIUC
import Client.IM.UserMenu as CIU
import Client.IM.WebSocket (WebSocket, onClose, onMessage, onOpen)
import Client.IM.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array ((!!))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Either (fromRight) as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..), (:>))
import Flame as F
import Flame.External as FE
import Foreign as FO
import Partial.Unsafe as UP
import Shared.DateTime (epoch)
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Window as WHW

main :: Effect Unit
main = do
      webSocket <- CIW.createWebSocket
      --web socket needs to be a ref as any time the connection can be closed and recreated by events
      webSocketRef <- ER.new webSocket
      fileReader <- WFR.fileReader
      channel <- F.resumeMount (QuerySelector ".im") {
            view: SIV.view true,
            init: [],
            update: update { fileReader, webSocketRef }
      }

      setUpWebSocket webSocketRef channel
      --for drag and drop
      CCF.setUpBase64Reader fileReader (DA.singleton <<< SetSelectedImage <<< Just) channel
      --receive profile edition changes
      CCD.addCustomEventListener nameChanged (SC.send channel <<< DA.singleton <<< SetNameFromProfile)
      --display settings/profile page
      FE.send [FE.onClick' [ShowUserContextMenu]] channel
      --image upload
      input <- CIC.getFileInput
      CCF.setUpFileChange (DA.singleton <<< SetSelectedImage <<< Just) input channel

      windowsFocus channel

update :: _ -> ListUpdate IMModel IMMessage
update { webSocketRef, fileReader} model =
      case _ of
            --chat
            InsertLink -> CIC.insertLink model
            ToggleChatModal modal -> CIC.toggleModal modal model
            DropFile event -> CIC.catchFile fileReader event model
            EnterBeforeSendMessage event -> CIC.enterBeforeSendMessage event model
            ForceBeforeSendMessage -> CIC.forceBeforeSendMessage model
            BeforeSendMessage content -> CIC.beforeSendMessage content model
            SendMessage date -> CIC.sendMessage webSocket date model
            SetMessageContent cursor content -> CIC.setMessage cursor content model
            Apply markup -> CIC.applyMarkup markup model
            SetSelectedImage maybeBase64 -> CIC.setSelectedImage maybeBase64 model
            ToggleMessageEnter -> CIC.toggleMessageEnter model
            SetEmoji event -> CIC.setEmoji event model
            --contacts
            ResumeChat id -> CICN.resumeChat id model
            MarkAsRead -> CICN.markRead webSocket model
            UpdateReadCount -> CICN.markRead webSocket model
            CheckFetchContacts -> CICN.checkFetchContacts model
            FetchContacts shouldFetch -> CICN.fetchContacts shouldFetch model
            DisplayContacts contacts -> CICN.displayContacts contacts model
            ResumeMissedEvents missed -> CICN.resumeMissedEvents missed model
            --history
            CheckFetchHistory -> CIH.checkFetchHistory model
            FetchHistory shouldFetch -> CIH.fetchHistory shouldFetch model
            DisplayHistory history -> CIH.displayHistory history model
            --suggestion
            ToggleContactProfile -> CIS.toggleContactProfile model
            PreviousSuggestion -> CIS.previousSuggestion model
            BlockUser id -> CIS.blockUser webSocket id model
            NextSuggestion -> CIS.nextSuggestion model
            DisplayMoreSuggestions suggestions -> CIS.displayMoreSuggestions suggestions model
            --user menu
            Logout -> CIU.logout model
            ShowUserContextMenu event -> CIU.showUserContextMenu event model
            ToggleModal toggle -> CIU.toggleModal toggle model
            SetModalContents file root html -> CIU.setModalContents file root html model
            SetUserContentMenuVisible toggle -> CIU.toogleUserContextMenu toggle model
            --main
            AlertUnreadChats -> CIUC.alertUnreadChats model
            ReceiveMessage payload isFocused -> receiveMessage webSocket isFocused payload model
            SetNameFromProfile name -> setName name model
            PreventStop event -> preventStop event model
            ToggleConnected -> toggleConnectedWebSocket model
            CheckMissedMessages -> checkMissedMessages model
            SetField setter -> F.noMessages $ setter model
      where webSocket = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e

receiveMessage :: WebSocket -> Boolean -> WebSocketPayloadClient -> IMModel -> MoreMessages
receiveMessage webSocket isFocused wsPayload model@{
      user: { id: recipientID },
      contacts,
      suggestions,
      blockedUsers
} = case wsPayload of
      ServerReceivedMessage { previousID, id, userID } ->
            F.noMessages $ model {
                  contacts = updateTemporaryID contacts userID previousID id
            }
      BeenBlocked { id } ->
            F.noMessages $ CIS.removeBlockedUser id model
      NewIncomingMessage payload@{ userID } ->
            if DA.elem userID blockedUsers then
                  F.noMessages model
            else case processIncomingMessage payload model of
                  Left userID -> model :> [Just <<< DisplayContacts <$> CCNT.response (request.im.singleContact { query: { id: userID }})]
                  Right updatedModel@{
                        chatting: Just index,
                        contacts
                  } ->  --mark it as read if we received a message from the current chat
                        let fields = {
                              chatting: index,
                              userID: recipientID,
                              contacts,
                              webSocket
                        }
                        in
                              if isFocused && isChatting userID fields then
                                    CICN.updateReadHistory updatedModel fields
                               else
                                    CIUC.alertUnreadChats updatedModel
                  Right updatedModel -> F.noMessages updatedModel
      PayloadError payload -> case payload of
            OutgoingMessage { id, userID } -> F.noMessages $ model {
                 contacts = updateHistoryStatus contacts userID id
            }
            --the connection might still be open and the server haven't saved the socket
            Connect -> CIF.nothingNext model <<< liftEffect $ CIW.close webSocket
            _ -> F.noMessages model
      where isChatting senderID { contacts, chatting } =
                  let ({ user: { id: recipientID } }) = contacts !@ chatting in
                  recipientID == senderID

processIncomingMessage :: ClientMessagePayload -> IMModel -> Either PrimaryKey IMModel
processIncomingMessage { id, userID, date, content } model@{
      user: { id: recipientID },
      suggestions,
      contacts,
      suggesting,
      chatting
} = case findAndUpdateContactList of
      Just contacts' ->
            Right $ model {
                  contacts = contacts'
            }
      Nothing -> Left userID
      where updateHistory { id, content, date } user@{ history } =
                  user {
                        history = DA.snoc history $  {
                              status: Received,
                              sender: userID,
                              recipient: recipientID,
                              id,
                              content,
                              date
                        }
                  }
            findAndUpdateContactList = do
                  index <- DA.findIndex findUser contacts
                  { history } <- contacts !! index
                  updated <- DA.modifyAt index (updateHistory { content, id, date }) contacts
                  pure updated

            findUser ({ user: { id } }) = userID == id

updateTemporaryID :: Array Contact -> PrimaryKey -> PrimaryKey -> PrimaryKey -> Array Contact
updateTemporaryID contacts userID previousMessageID messageID = updateContactHistory contacts userID updateTemporary
      where updateTemporary history@( { id })
                  | id == previousMessageID = history { id = messageID, status = Received }
                  | otherwise = history

updateHistoryStatus :: Array Contact -> PrimaryKey -> PrimaryKey -> Array Contact
updateHistoryStatus contacts userID messageID = updateContactHistory contacts userID updateStatus
      where updateStatus history@( { id })
                  | id == messageID = history { status = Errored }
                  | otherwise = history

updateContactHistory :: Array Contact -> PrimaryKey -> (HistoryMessage -> HistoryMessage) -> Array Contact
updateContactHistory contacts userID f = updateContact <$> contacts
      where updateContact contact@{ user: { id }, history }
                  | id == userID = contact {
                        history = f <$> history
                  }
                  | otherwise = contact

checkMissedMessages :: IMModel -> MoreMessages
checkMissedMessages model@{ contacts, user : { id: senderID } } =
      model :> [do
            let lastSenderID = findLast (\h -> senderID == h.sender && h.status == Received) contacts
                lastRecipientID = findLast ((senderID /= _) <<< _.sender) contacts

            if DM.isNothing lastSenderID && DM.isNothing lastRecipientID then
                  pure Nothing
             else
                  Just <<< ResumeMissedEvents <$> CCNT.response (request.im.missedEvents { query: { lastSenderID, lastRecipientID } })
      ]
      where findLast f array = do
                  { history } <- DA.head array
                  index <- DA.findLastIndex f history
                  { id } <- history !! index
                  pure id

setName :: String -> IMModel -> NoMessages
setName name model@{ user } =
      F.noMessages $ model {
            user = user {
                  name = name
            }
      }

toggleConnectedWebSocket :: IMModel -> NoMessages
toggleConnectedWebSocket model@{ isWebSocketConnected } =
      F.noMessages $ model {
            hasTriedToConnectYet = true,
            isWebSocketConnected = not isWebSocketConnected
      }

preventStop :: Event -> IMModel -> NextMessage
preventStop event model = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event

windowsFocus :: Channel (Array IMMessage) -> Effect Unit
windowsFocus channel = do
      focusListener <- WET.eventListener $ const (SC.send channel $ DA.singleton UpdateReadCount)
      --focus event has to be on the window as chrome is a whiny baby about document
      window <- WH.window
      WET.addEventListener focus focusListener false $ WHW.toEventTarget window

setUpWebSocket :: Ref WebSocket -> Channel (Array IMMessage) -> Effect Unit
setUpWebSocket webSocketRef channel = do
      webSocket <- ER.read webSocketRef
      let webSocketTarget = CIW.toEventTarget webSocket
      --a ref is used to track reconnections
      timerID <- ER.new Nothing
      openListener <- WET.eventListener $ const (do
            CIW.sendPayload webSocket Connect
            sendChannel ToggleConnected)
      messageListener <- WET.eventListener $ \event -> do
            maybeID <- ER.read timerID
            DM.maybe (pure unit) (\id -> do
                  ET.clearTimeout id
                  ER.write Nothing timerID) maybeID
            let payload = SU.fromRight <<< CME.runExcept <<< FO.readString <<< CIW.data_ <<< SU.fromJust $ CIW.fromEvent event
                message = SU.fromRight $ SJ.fromJSON payload
            isFocused <- CCD.documentHasFocus
            sendChannel $ ReceiveMessage message isFocused

      closeListener <- WET.eventListener $ \_ -> do
            sendChannel ToggleConnected
            maybeID <- ER.read timerID
            when (DM.isNothing maybeID) do
                  milliseconds <- ERD.randomInt 2000 7000
                  id <- ET.setTimeout milliseconds <<< void $ do
                        newWebSocket <- CIW.createWebSocket
                        ER.write newWebSocket webSocketRef
                        setUpWebSocket webSocketRef channel
                        sendChannel CheckMissedMessages
                  ER.write (Just id) timerID

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget
      where sendChannel = SC.send channel <<< DA.singleton
