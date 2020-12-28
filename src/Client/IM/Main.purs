module Client.IM.Main where

import Prelude
import Shared.Types

import Client.Common.DOM (nameChanged, notificationClick)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Client.Common.Network as CCNT
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame (MoreMessages, NextMessage, NoMessages)
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Notification as CIUC
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Client.IM.WebSocket (WebSocket, onClose, onMessage, onOpen)
import Client.IM.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HS
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Traversable as DT
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..), (:>))
import Flame as F
import Flame.Html.Signal as FE
import Foreign as FO
import Shared.Breakpoint (mobileBreakpoint)
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW

main :: Effect Unit
main = do
      webSocket <- CIW.createWebSocket
      --web socket needs to be a ref as any time the connection can be closed and recreated by events
      webSocketRef <- ER.new webSocket
      --REFACTOR: use bounds
      elements <- cacheElements [ImageFileInput, ChatInput, ImageFormCaption, MessageHistory, Favicon, ProfileEditionRoot, SettingsEditionRoot, KarmaLeaderboard, HelpRoot]
      fileReader <- WFR.fileReader
      channel <- F.resumeMount (QuerySelector ".im") {
            view: SIV.view true,
            init: [],
            update: update { fileReader, webSocketRef, elements }
      }
      setUpWebSocket webSocketRef channel
      --for drag and drop
      CCF.setUpBase64Reader fileReader (DA.singleton <<< SetSelectedImage <<< Just) channel
      --receive profile edition changes
      CCD.addCustomEventListener nameChanged (SC.send channel <<< DA.singleton <<< SetNameFromProfile)
      --move to given chat when clicking on system notification
      CCD.addCustomEventListener notificationClick (SC.send channel <<< DA.singleton <<< ResumeChat)
      --display settings/profile page
      FE.send [FE.onClick' [ToggleUserContextMenu]] channel
      --image upload
      input <- CIC.getFileInput
      CCF.setUpFileChange (DA.singleton <<< SetSelectedImage <<< Just) input channel
      width <- CCD.screenWidth
      --keep track of mobile (-like) screens for things that cant be done with media queries
      when (width < mobileBreakpoint) $ SC.send channel [SetSmallScreen]
      checkNotifications channel
      windowsFocus channel

update :: _ -> ListUpdate IMModel IMMessage
update { webSocketRef, fileReader, elements } model =
      case _ of
            --chat
            InsertLink -> CIC.insertLink model
            ToggleChatModal modal -> CIC.toggleModal modal model
            DropFile event -> CIC.catchFile fileReader event model
            EnterBeforeSendMessage -> CIC.enterBeforeSendMessage model
            ForceBeforeSendMessage -> CIC.forceBeforeSendMessage model
            ResizeChatInput event -> CIC.resizeChatInput event model
            BeforeSendMessage content -> CIC.beforeSendMessage content model
            SendMessage date -> CIC.sendMessage webSocket date model
            SetMessageContent cursor content -> CIC.setMessage cursor content model
            Apply markup -> CIC.applyMarkup markup model
            SetSelectedImage maybeBase64 -> CIC.setSelectedImage maybeBase64 model
            SetSmallScreen -> setSmallScreen model
            SetEmoji event -> CIC.setEmoji event model
            ToggleMessageEnter -> CIC.toggleMessageEnter model
            FocusInput selector -> focusInput selector model
            --contacts
            ResumeChat id -> CICN.resumeChat id model
            MarkAsRead -> CICN.markRead webSocket model
            UpdateReadCount -> CICN.markRead webSocket model
            CheckFetchContacts -> CICN.checkFetchContacts model
            SpecialRequest (FetchContacts shouldFetch) -> CICN.fetchContacts shouldFetch model
            DisplayContacts contacts -> CICN.displayContacts contacts model
            DisplayNewContacts contacts -> CICN.displayNewContacts contacts model
            ResumeMissedEvents missed -> CICN.resumeMissedEvents missed model
            --history
            CheckFetchHistory -> CIH.checkFetchHistory model
            SpecialRequest (FetchHistory shouldFetch) -> CIH.fetchHistory shouldFetch model
            DisplayHistory history -> CIH.displayHistory history model
            --suggestion
            ResumeSuggesting -> CIS.resumeSuggesting model
            ToggleContactProfile -> CIS.toggleContactProfile model
            SpecialRequest PreviousSuggestion -> CIS.previousSuggestion model
            SpecialRequest NextSuggestion -> CIS.nextSuggestion model
            SpecialRequest (BlockUser id) -> CIS.blockUser webSocket id model
            DisplayMoreSuggestions suggestions -> CIS.displayMoreSuggestions suggestions model
            --user menu
            ToggleInitialScreen -> CIU.toggleInitialScreen model
            Logout -> CIU.logout model
            ToggleUserContextMenu event -> toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) -> CIU.toggleModal toggle model
            SetModalContents file root html -> CIU.setModalContents file root html model
            SetContextMenuToggle toggle -> CIU.toogleUserContextMenu toggle model
            --main
            ReceiveMessage payload isFocused -> receiveMessage webSocket isFocused payload model
            SetNameFromProfile name -> setName name model
            AskNotification -> askNotification model
            ToggleAskNotification -> toggleAskNotification model
            PreventStop event -> preventStop event model
            ToggleConnected isConnected -> toggleConnectedWebSocket isConnected model
            SpecialRequest CheckMissedEvents -> checkMissedEvents model
            SetField setter -> F.noMessages $ setter model
            ToggleFortune isVisible -> toggleFortune isVisible model
            DisplayFortune sequence -> displayFortune sequence model
            RequestFailed failure -> addFailure failure model
      where webSocket = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e

askNotification :: IMModel -> MoreMessages
askNotification model = CIF.nothingNext (model { enableNotificationsVisible = false }) $ liftEffect CCD.requestNotificationPermission

--refactor: all messages like this can be dryed into a single function
toggleAskNotification :: IMModel -> NoMessages
toggleAskNotification model@{ enableNotificationsVisible } = F.noMessages $ model {
      enableNotificationsVisible = not enableNotificationsVisible
}

toggleUserContextMenu :: Event -> IMModel -> MoreMessages
toggleUserContextMenu event model@{ toggleContextMenu }
      | toggleContextMenu /= HideContextMenu =
            F.noMessages $ model { toggleContextMenu = HideContextMenu }
      | otherwise =
            model :> [
                  liftEffect <<< map (Just <<< SetContextMenuToggle <<< toggle) $ WDE.id <<< SU.fromJust $ do
                  target <- WEE.target event
                  WDE.fromEventTarget target
            ]
      where toggle a= case a of
                  "user-context-menu" -> ShowUserContextMenu
                  "suggestion-context-menu" -> ShowSuggestionContextMenu
                  "compact-profile-context-menu" -> ShowCompactProfileContextMenu
                  "full-profile-context-menu" -> ShowFullProfileContextMenu
                  _ -> HideContextMenu

focusInput :: String -> IMModel -> NextMessage
focusInput selector model = model :> [
      liftEffect do
            element <- CCD.querySelector selector
            WHHE.focus $ SU.fromJust do
                  e <- element
                  WHHE.fromElement e
            pure Nothing
]

addFailure :: RequestFailure -> IMModel -> NoMessages
addFailure failure@{ request } model@{ failedRequests, errorMessage } = F.noMessages $ model {
      failedRequests = failure : failedRequests,
      errorMessage = case request of
            BlockUser _ -> "Could not block user. Please try again"
            PreviousSuggestion -> suggestionsError
            NextSuggestion -> suggestionsError
            _ -> errorMessage
}
      where suggestionsError = "Could not fetch suggestions. Please try again"

toggleFortune :: Boolean -> IMModel -> MoreMessages
toggleFortune isVisible model
      | isVisible = model :> [Just <<< DisplayFortune <$> CCNT.silentResponse (request.im.fortune {})]
      | otherwise = F.noMessages $ model {
            fortune = Nothing
      }

displayFortune :: String -> IMModel -> NoMessages
displayFortune sequence model = F.noMessages $ model {
      fortune = Just sequence
}

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
            F.noMessages <<< unsuggest id $ model { contacts = markContactUnavailable contacts id }
      NewIncomingMessage payload@{ userID } ->
            if DA.elem userID blockedUsers then
                  F.noMessages model
            else let model' = unsuggest userID model in
                  case processIncomingMessage payload model' of
                        Left userID -> model' :> [CCNT.retryableResponse CheckMissedEvents DisplayNewContacts (request.im.singleContact { query: { id: userID }})]
                        Right updatedModel@{
                              chatting: Just index,
                              contacts
                        } | isFocused && isChatting userID updatedModel ->  --mark it as read if we received a message from the current chat
                              CICN.updateReadHistory updatedModel {
                                    chatting: index,
                                    userID: recipientID,
                                    contacts,
                                    webSocket
                              }
                        Right updatedModel -> CIUC.notifyUnreadChats updatedModel [payload.userID]
      PayloadError payload -> case payload.origin of
            OutgoingMessage { id, userID } -> F.noMessages $ model {
                 contacts =
                        --assume that it is because the other user no longer exists
                        if payload.context == Just MissingForeignKey then
                              markContactUnavailable contacts userID
                         else
                              markErroredMessage contacts userID id
            }
            --the connection might still be open and the server haven't saved the socket
            Connect -> CIF.nothingNext model <<< liftEffect $ CIW.close webSocket
            _ -> F.noMessages model
      where isChatting senderID { contacts, chatting } =
                  let ({ user: { id: recipientID } }) = contacts !@ (SU.fromJust chatting) in
                  recipientID == senderID

unsuggest :: PrimaryKey -> IMModel -> IMModel
unsuggest userID model@{ suggestions, suggesting } = model {
      suggestions = DA.filter ((userID /= _) <<< _.id) suggestions,
      suggesting = (\i -> if i == 0 then 0  else i - 1) <$> suggesting
}

processIncomingMessage :: ClientMessagePayload -> IMModel -> Either PrimaryKey IMModel
processIncomingMessage { id, userID, date, content } model@{
      user: { id: recipientID },
      suggestions,
      contacts,
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

markErroredMessage :: Array Contact -> PrimaryKey -> PrimaryKey -> Array Contact
markErroredMessage contacts userID messageID = updateContactHistory contacts userID updateStatus
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

markContactUnavailable :: Array Contact -> PrimaryKey -> Array Contact
markContactUnavailable contacts userID = updateContact <$> contacts
      where updateContact contact@{ user: { id }, history }
                  | id == userID = contact {
                        available = false
                  }
                  | otherwise = contact

checkMissedEvents :: IMModel -> MoreMessages
checkMissedEvents model@{ contacts, user : { id: senderID } } =
      model :> [do
            let lastSenderID = findLast (\h -> senderID == h.sender && h.status == Received) contacts
                lastRecipientID = findLast ((senderID /= _) <<< _.sender) contacts

            if DM.isNothing lastSenderID && DM.isNothing lastRecipientID then
                  pure Nothing
             else
                  CCNT.retryableResponse CheckMissedEvents ResumeMissedEvents (request.im.missedEvents { query: { lastSenderID, lastRecipientID } })
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

toggleConnectedWebSocket :: Boolean -> IMModel -> MoreMessages
toggleConnectedWebSocket isConnected model@{ hasTriedToConnectYet, isWebSocketConnected, errorMessage } =
      model {
            hasTriedToConnectYet = true,
            isWebSocketConnected = isConnected,
            errorMessage = if not isConnected then lostConnectionMessage else if errorMessage == lostConnectionMessage then "" else errorMessage
      } :> if hasTriedToConnectYet && isConnected then [pure <<< Just $ SpecialRequest CheckMissedEvents] else []
      where lostConnectionMessage = "Connection to the server lost. Attempting to automaticaly reconnect..."

preventStop :: Event -> IMModel -> NextMessage
preventStop event model = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event

checkNotifications :: Channel (Array IMMessage) -> Effect Unit
checkNotifications channel = do
      status <- CCD.notificationPermission
      when (status /= "granted") $ SC.send channel [ToggleAskNotification]

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
            sendChannel $ ToggleConnected true)
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
            sendChannel $ ToggleConnected false
            maybeID <- ER.read timerID
            when (DM.isNothing maybeID) do
                  milliseconds <- ERD.randomInt 2000 7000
                  id <- ET.setTimeout milliseconds <<< void $ do
                        newWebSocket <- CIW.createWebSocket
                        ER.write newWebSocket webSocketRef
                        setUpWebSocket webSocketRef channel
                  ER.write (Just id) timerID

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget
      where sendChannel = SC.send channel <<< DA.singleton

setSmallScreen :: IMModel -> NoMessages
setSmallScreen model@{ messageEnter } =
      F.noMessages $ model {
            messageEnter = false,
            smallScreen = true
      }

cacheElements :: Array IMSelector -> Effect (HashMap IMSelector Element)
cacheElements selectors = do
      nodes <- DT.traverse ((SU.fromJust <$> _) <$> CCD.querySelector <<< show) selectors
      pure <<< HS.fromArray $ DA.zip selectors nodes