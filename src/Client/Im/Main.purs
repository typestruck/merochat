module Client.Im.Main where

--refactor: this file needs to be broken down into modules

import Debug
import Prelude
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.User

import Client.Common.Dom (setChatExperiment)
import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Common.Network as CNN
import Client.Common.Types (CurrentWebSocket)
import Client.Im.Chat as CIC
import Client.Im.Contacts as CICN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Flame as CIF
import Client.Im.History as CIH
import Client.Im.Notification as CIUC
import Client.Im.Scroll as CISM
import Client.Im.Suggestion as CIS
import Client.Im.UserMenu as CIU
import Client.Im.WebSocket (WebSocket, onClose, onMessage, onOpen)
import Client.Im.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol as DST
import Data.Symbol as TDS
import Data.Time.Duration (Days(..), Milliseconds(..))
import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Now as EN
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..), (:>))
import Flame as F
import Flame.Subscription as FS
import Flame.Subscription.Document as FSD
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Flame.Subscription.Window as FSW
import Foreign as FO
import Safe.Coerce as SC
import Shared.Breakpoint (mobileBreakpoint)
import Shared.Element (ElementId(..))
import Shared.Im.View as SIV
import Shared.Json as SJ
import Shared.Network (RequestStatus(..))
import Shared.Options.MountPoint (imId, profileId)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Profile.Types (ProfileMessage(..))
import Shared.ResponseError (DatabaseError(..))
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Shared.User as SUR
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW

main ∷ Effect Unit
main = do
      webSocket ← CIW.createWebSocket
      --web socket needs to be a ref as any time the connection can be closed and recreated by events
      webSocketRef ← ER.new { webSocket, ponged: true }
      fileReader ← WFR.fileReader
      F.resumeMount (QuerySelector ".im") imId
            { view: SIV.view true
            , subscribe:
                    [
                      --display settings/profile/etc page menus
                      FSD.onClick' ToggleUserContextMenu
                    ,
                      --focus event has to be on the window as chrome is a whiny baby about document
                      FSW.onFocus UpdateReadCount
                    ,
                      --events from chat experiment
                      FS.onCustomEvent setChatExperiment SetChatExperiment
                    ]
            , init: []
            , update: update { fileReader, webSocketRef }
            }
      setUpWebSocket webSocketRef
      width ← CCD.screenWidth
      let smallScreen = width < mobileBreakpoint
      if smallScreen then
            FS.send imId SetSmallScreen --keep track of mobile (-like) screens for things that cant be done with media queries
      else
            checkNotifications --notification permission (desktop)
      --disable the back button on desktop/make the back button go back to previous screen on mobile
      CCD.pushState $ routes.im.get {}
      historyChange smallScreen
      --for drag and drop
      CCF.setUpBase64Reader fileReader (SetSelectedImage <<< Just) imId
      --image upload
      input ← CCD.unsafeGetElementById ImageFileInput
      CCF.setUpFileChange (SetSelectedImage <<< Just) input imId
      --harass temporary users on their last day to make an account
      FS.send imId CheckUserExpiration

update ∷ _ → ListUpdate ImModel ImMessage
update { webSocketRef, fileReader } model =
      case _ of
            --chat
            InsertLink → CIC.insertLink model
            ToggleChatModal modal → CIC.toggleModal modal model
            DropFile event → CIC.catchFile fileReader event model
            EnterBeforeSendMessage event → CIC.enterBeforeSendMessage event model
            ForceBeforeSendMessage → CIC.forceBeforeSendMessage model
            ResizeChatInput event → CIC.resizeChatInput event model
            BeforeSendMessage content → CIC.beforeSendMessage content model
            SendMessage content date → CIC.sendMessage webSocket content date model
            SetMessageContent cursor content → CIC.setMessage cursor content model
            SetSelectedImage maybeBase64 → CIC.setSelectedImage maybeBase64 model
            Apply markup → CIC.applyMarkup markup model
            SetSmallScreen → setSmallScreen model
            SetEmoji event → CIC.setEmoji event model
            ToggleMessageEnter → CIC.toggleMessageEnter model
            FocusCurrentSuggestion → CIC.focusCurrentSuggestion model
            FocusInput elementId → focusInput elementId model
            QuoteMessage message event → CIC.quoteMessage message event model
            CheckTyping text → CIC.checkTyping text (EU.unsafePerformEffect EN.nowDateTime) webSocket model
            NoTyping id → F.noMessages $ CIC.updateTyping id false model
            TypingId id → F.noMessages model { typingIds = DA.snoc model.typingIds $ SC.coerce id }
            --contacts
            ResumeChat (Tuple id impersonating) → CICN.resumeChat id impersonating model
            UpdateDelivered → CICN.markDelivered webSocket model
            UpdateReadCount → CICN.markRead webSocket model
            CheckFetchContacts → CICN.checkFetchContacts model
            SpecialRequest (FetchContacts shouldFetch) → CICN.fetchContacts shouldFetch model
            SpecialRequest (DeleteChat tupleId) → CICN.deleteChat tupleId model
            DisplayContacts contacts → CICN.displayContacts contacts model
            DisplayNewContacts contacts → CICN.displayNewContacts contacts model
            DisplayImpersonatedContact id history contacts → CICN.displayImpersonatedContacts id history contacts model
            ResumeMissedEvents missed → CICN.resumeMissedEvents missed model
            --history
            CheckFetchHistory → CIH.checkFetchHistory model
            SpecialRequest (FetchHistory shouldFetch) → CIH.fetchHistory shouldFetch model
            DisplayHistory history → CIH.displayHistory history model
            --suggestion
            FetchMoreSuggestions → CIS.fetchMoreSuggestions model
            ResumeSuggesting → CIS.resumeSuggesting model
            ToggleContactProfile → CIS.toggleContactProfile model
            SpecialRequest PreviousSuggestion → CIS.previousSuggestion model
            SpecialRequest NextSuggestion → CIS.nextSuggestion model
            SpecialRequest (BlockUser id) → CIS.blockUser webSocket id model
            DisplayMoreSuggestions suggestions → CIS.displayMoreSuggestions suggestions model
            --user menu
            ToggleInitialScreen toggle → CIU.toggleInitialScreen toggle model
            Logout → CIU.logout model
            ToggleUserContextMenu event → toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) → CIU.toggleModal toggle model
            SetModalContents file root html → CIU.setModalContents file root html model
            SetContextMenuToggle toggle → CIU.toggleUserContextMenu toggle model
            --main
            AskChatExperiment → askExperiment model
            SetChatExperiment experiment → setExperiment experiment model
            ReloadPage → reloadPage model
            ReceiveMessage payload isFocused → receiveMessage webSocket isFocused payload model
            SetNameFromProfile name → setName name model
            SetAvatarFromProfile base64 → setAvatar base64 model
            AskNotification → askNotification model
            ToggleAskNotification → toggleAskNotification model
            CreateUserFromTemporary → registerUser model
            PreventStop event → preventStop event model
            CheckUserExpiration → checkUserExpiration model
            FinishTutorial → finishTutorial model
            ToggleConnected isConnected → toggleConnectedWebSocket isConnected model
            TerminateTemporaryUser → terminateAccount model
            SpecialRequest CheckMissedEvents → checkMissedEvents model
            SetField setter → F.noMessages $ setter model
            ToggleFortune isVisible → toggleFortune isVisible model
            DisplayFortune sequence → displayFortune sequence model
            RequestFailed failure → addFailure failure model
            SpecialRequest (ReportUser userId) → report userId webSocket model
            SendPing isActive → sendPing webSocket isActive model
            SetRegistered → setRegistered model
            SetPrivacySettings ps → setPrivacySettings ps model
            DisplayAvailability availability → displayAvailability availability model
      where
      { webSocket } = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e

displayAvailability ∷ AvailabilityStatus → ImModel → NoMessages
displayAvailability avl model@{ contacts, suggestions } = F.noMessages $ model
      { contacts = map updateContact contacts
      , suggestions = map updateUser suggestions
      }
      where
      availability = DH.fromArray $ map (\{ id, status } → Tuple id status) avl
      updateContact contact@{ user: { id } } = case DH.lookup id availability of
            Just status → contact { user { availability = status } }
            Nothing → contact
      updateUser user@{ id } = case DH.lookup id availability of
            Just status → user { availability = status }
            Nothing → user

setRegistered ∷ ImModel → NoMessages
setRegistered model = model { user { temporary = false } } :>
      [ do
              liftEffect $ FS.send profileId AfterRegistration
              pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile
      ]

registerUser ∷ ImModel → MoreMessages
registerUser model@{ temporaryEmail, temporaryPassword, erroredFields } =
      if invalidEmail then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol (Proxy ∷ _ "temporaryEmail")  }
      else if invalidPassword then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol  (Proxy ∷ _ "temporaryPassword")  }
      else
            model { erroredFields = [] } :>
                  [ do
                          status ← CCN.formRequest (show TemporaryUserSignUpForm) $ request.im.register { body: { email: SU.fromJust temporaryEmail, password: SU.fromJust temporaryPassword } }
                          case status of
                                Failure → pure Nothing
                                Success → pure $ Just SetRegistered
                  ]
      where
      invalidEmail = DM.maybe true (\email → DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email)) temporaryEmail
      invalidPassword = DM.maybe true (\password → DS.length password < passwordMinCharacters) temporaryPassword

terminateAccount ∷ ImModel → NextMessage
terminateAccount model = model :>
      [ do
              status ← CNN.formRequest (show ConfirmAccountTerminationForm) $ request.settings.account.terminate { body: {} }
              when (status == Success) $ do
                    EA.delay $ Milliseconds 3000.0
                    liftEffect <<< CCL.setLocation $ routes.login.get {}
              pure Nothing
      ]

checkUserExpiration ∷ ImModel → MoreMessages
checkUserExpiration model@{ user: { temporary, joined } }
      | temporary && SUR.temporaryUserExpiration joined <= Days 1.0 = model :> [ pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile ]
      | otherwise = F.noMessages model

sendPing ∷ WebSocket → Boolean → ImModel → NoMessages
sendPing webSocket isActive model@{ contacts, suggestions } =
      CIF.nothingNext model <<< liftEffect <<< CIW.sendPayload webSocket $ Ping
            { isActive
            , statusFor: map _.id suggestions <> map (_.id <<< _.user) (DA.filter ((_ /= Unavailable) <<< _.availability <<< _.user) contacts)
            }

setPrivacySettings ∷ PrivacySettings → ImModel → NextMessage
setPrivacySettings { readReceipts, typingStatus, profileVisibility, onlineStatus, messageTimestamps } model =
      model
            { user
                    { profileVisibility = profileVisibility
                    , readReceipts = readReceipts
                    , typingStatus = typingStatus
                    , onlineStatus = onlineStatus
                    , messageTimestamps = messageTimestamps
                    }
            } :> [ pure $ Just FetchMoreSuggestions ]

finishTutorial ∷ ImModel → NextMessage
finishTutorial model@{ toggleModal } = model { user { completedTutorial = true } } :> [ finish ]
      where
      finish = do
            void <<< CCNT.silentResponse $ request.im.tutorial {}
            if toggleModal == Tutorial OptionsMenu then --user might have navigated to other modals

                  pure <<< Just <<< SpecialRequest $ ToggleModal HideUserMenuModal
            else pure Nothing

report ∷ Int → WebSocket → ImModel → MoreMessages
report userId webSocket model@{ reportReason, reportComment } = case reportReason of
      Just rs →
            CIS.updateAfterBlock userId
                  ( model
                          { reportReason = Nothing
                          , reportComment = Nothing
                          }
                  ) :>
                  [ do
                          result ← CCN.defaultResponse $ request.im.report { body: { userId, reason: rs, comment: reportComment } }
                          case result of
                                Left _ → pure <<< Just $ RequestFailed { request: ReportUser userId, errorMessage: Nothing }
                                _ → do
                                      liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id: userId }
                                      pure Nothing
                  ]
      Nothing → F.noMessages $ model
            { erroredFields = [ TDS.reflectSymbol (Proxy ∷ Proxy "reportReason") ]
            }

askExperiment ∷ ImModel → MoreMessages
askExperiment model@{ experimenting } = model :>
      if DM.isNothing experimenting then []
      else
            [ do
                    liftEffect $ FSUC.broadcast setChatExperiment experimenting
                    pure Nothing
            ]

setExperiment ∷ Maybe ExperimentData → ImModel → MoreMessages
setExperiment experiment model@{ toggleModal, contacts, experimenting, suggestionsPage } =
      model
            { chatting = Nothing
            , temporaryId = 3000000
            , contacts = if impersonating then [] else contacts
            , experimenting = experiment
            , toggleModal = if toggleModal == ShowExperiments then HideUserMenuModal else toggleModal
            , suggestionsPage = if impersonating then 0 else suggestionsPage
            } :> if impersonating then [ pure $ Just FetchMoreSuggestions ] else []
      where
      impersonating = case experiment of
            imp@(Just (Impersonation (Just _))) → imp /= experimenting --avoid running more than once
            _ → false

reloadPage ∷ ImModel → NextMessage
reloadPage model = CIF.nothingNext model $ liftEffect CCL.reload

askNotification ∷ ImModel → MoreMessages
askNotification model = CIF.nothingNext (model { enableNotificationsVisible = false }) $ liftEffect CCD.requestNotificationPermission

--refactor: all messages like this can be dryed into a single function
toggleAskNotification ∷ ImModel → NoMessages
toggleAskNotification model@{ enableNotificationsVisible } = F.noMessages $ model
      { enableNotificationsVisible = not enableNotificationsVisible
      }

toggleUserContextMenu ∷ Event → ImModel → MoreMessages
toggleUserContextMenu event model@{ toggleContextMenu }
      | toggleContextMenu /= HideContextMenu =
              F.noMessages $ model { toggleContextMenu = HideContextMenu }
      | otherwise =
              model :>
                    [
                      --we cant use node.contains as some of the elements are dynamically created/destroyed
                      liftEffect do
                            let
                                  element = SU.fromJust $ do
                                        target ← WEE.target event
                                        WDE.fromEventTarget target
                            id ← WDE.id element
                            parent ← WDN.parentElement $ WDE.toNode element
                            parentId ← case parent of
                                  Just e → WDE.id e
                                  Nothing → pure ""
                            pure <<< Just <<< SetContextMenuToggle $ toggle id parentId
                    ]
              where
              toggle elementId parentId
                    | elementId == show UserContextMenu || parentId == show UserContextMenu = ShowUserContextMenu
                    | elementId == show SuggestionContextMenu || parentId == show SuggestionContextMenu = ShowSuggestionContextMenu
                    | elementId == show CompactProfileContextMenu || parentId == show CompactProfileContextMenu = ShowCompactProfileContextMenu
                    | elementId == show FullProfileContextMenu || parentId == show FullProfileContextMenu = ShowFullProfileContextMenu
                    | otherwise = HideContextMenu

focusInput ∷ ElementId → ImModel → NextMessage
focusInput elementId model = model :>
      [ liftEffect do
              element ← CCD.getElementById elementId
              WHHE.focus $ SU.fromJust do
                    e ← element
                    WHHE.fromElement e
              pure Nothing
      ]

addFailure ∷ RequestFailure → ImModel → NoMessages
addFailure failure@{ request } model@{ failedRequests, errorMessage } = F.noMessages $ model
      { failedRequests = failure : failedRequests
      , errorMessage = case request of
              BlockUser _ → "Could not block user. Please try again"
              ReportUser _ → "Could not report user. Please try again"
              PreviousSuggestion → suggestionsError
              NextSuggestion → suggestionsError
              _ → errorMessage
      }
      where
      suggestionsError = "Could not fetch suggestions. Please try again"

toggleFortune ∷ Boolean → ImModel → MoreMessages
toggleFortune isVisible model
      | isVisible = model :> [ Just <<< DisplayFortune <$> CCNT.silentResponse (request.im.fortune {}) ]
      | otherwise = F.noMessages $ model
              { fortune = Nothing
              }

displayFortune ∷ String → ImModel → NoMessages
displayFortune sequence model = F.noMessages $ model
      { fortune = Just sequence
      }

--refactor: this needs some serious cleanup
receiveMessage ∷ WebSocket → Boolean → WebSocketPayloadClient → ImModel → MoreMessages
receiveMessage
      webSocket
      isFocused
      wsPayload
      model@
            { user: { id: recipientId }
            , contacts: currentContacts
            , typingIds
            , hash
            , blockedUsers
            } = case wsPayload of
      CurrentHash newHash →
            F.noMessages $ model
                  { imUpdated = newHash /= hash
                  }
      ContactTyping { id } → CIC.updateTyping id true model :>
            [ liftEffect do
                    DT.traverse_ (ET.clearTimeout <<< SC.coerce) typingIds
                    newId ← ET.setTimeout 1000 <<< FS.send imId $ NoTyping id
                    pure <<< Just $ TypingId newId
            ]
      ServerReceivedMessage { previousId, id, userId } →
            F.noMessages $ model
                  { contacts = updateTemporaryId currentContacts userId previousId id
                  }
      ServerChangedStatus { ids, status, userId } →
            F.noMessages $ model
                  { contacts = updateStatus currentContacts userId ids status
                  }
      ContactUnavailable { userId, temporaryMessageId } →
            F.noMessages <<< unsuggest userId $ model
                  { contacts =
                          let
                                updatedContacts = markContactUnavailable currentContacts userId
                          in
                                case temporaryMessageId of
                                      Nothing → updatedContacts
                                      Just id → markErroredMessage updatedContacts userId id
                  }
      NewIncomingMessage payload@{ id: messageId, userId, content: messageContent, date: messageDate, experimenting } →
            --(for now) if the experiments don't match, discard the message
            if DA.elem userId blockedUsers || not (match userId experimenting) then
                  F.noMessages $ spy "aaa" model
            else
                  let
                        model' = unsuggest userId model
                  in
                        case processIncomingMessage payload model' of
                              Left userId →
                                    let
                                          message = case (spy "exp" experimenting) of
                                                Just (ImpersonationPayload { id: impersonationId }) →
                                                      DisplayImpersonatedContact impersonationId
                                                            { status: Received
                                                            , sender: userId
                                                            , recipient: recipientId
                                                            , id: messageId
                                                            , content: messageContent
                                                            , date: messageDate
                                                            }
                                                _ → DisplayNewContacts
                                    in
                                        spy "bbb"  model' :> [ CCNT.retryableResponse CheckMissedEvents message $ request.im.contact { query: { id: userId, impersonation: DM.isJust experimenting } } ]
                              --mark it as read if we received a message from the current chat
                              -- or as delivered otherwise
                              Right
                                    updatedModel@
                                          { chatting: Just index
                                          , contacts
                                          } | isFocused && isChatting userId updatedModel →
                                    let
                                          Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel
                                                { sessionUserId: recipientId
                                                , contacts
                                                , newStatus: Read
                                                , webSocket
                                                , index
                                                }
                                    in
                                          spy "ccc" furtherUpdatedModel :> (CISM.scrollLastMessage' : messages)
                              Right
                                    updatedModel@
                                          { contacts
                                          } →
                                    let
                                          impersonationId = case experimenting of
                                                Just (ImpersonationPayload { id }) → Just id
                                                _ → Nothing
                                          Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel
                                                { index: SU.fromJust $ DA.findIndex (findContact userId impersonationId model.experimenting) contacts
                                                , sessionUserId: recipientId
                                                , newStatus: Delivered
                                                , contacts
                                                , webSocket
                                                }
                                    in
                                        spy "ddd"  furtherUpdatedModel :> (CIUC.notify' furtherUpdatedModel [ Tuple payload.userId impersonationId ] : messages)

      PayloadError payload → case payload.origin of
            OutgoingMessage { id, userId } → F.noMessages $ model
                  { contacts =
                          --assume that it is because the other user no longer exists
                          if payload.context == Just MissingForeignKey then
                                markContactUnavailable currentContacts userId
                          else
                                markErroredMessage currentContacts userId id
                  }
            _ → F.noMessages model
      where
      isChatting senderID { contacts, chatting } =
            let
                  { user: { id: recipientId }, impersonating } = contacts !@ SU.fromJust chatting
            in
                  recipientId == senderID

      match userId experimenting = case model.experimenting, experimenting of
            Just (Impersonation (Just _)), Nothing → false
            Just (Impersonation (Just { id })), Just (ImpersonationPayload { id: otherID, sender }) → id == otherID && not sender
            Nothing, Just (ImpersonationPayload { sender }) → sender
            _, _ → true

unsuggest ∷ Int → ImModel → ImModel
unsuggest userId model@{ suggestions, suggesting } = model
      { suggestions = DA.filter ((userId /= _) <<< _.id) suggestions
      , suggesting = (\i → if i == 0 then 0 else i - 1) <$> suggesting
      }

processIncomingMessage ∷ ClientMessagePayload → ImModel → Either Int ImModel
processIncomingMessage
      { id, userId, date, content, experimenting }
      model@
            { user: { id: recipientId }
            , suggestions
            , contacts
            , chatting
            } = case findAndUpdateContactList of
      Just contacts' →
            Right $ model
                  { contacts = contacts'
                  }
      Nothing → Left userId
      where
      updateHistory { id, content, date } contact@{ history } =
            contact
                  { history = DA.snoc history $
                          { status: Received
                          , sender: userId
                          , recipient: recipientId
                          , id
                          , content
                          , date
                          }
                  }
      impersonationId = case experimenting of
            Just (ImpersonationPayload { id }) → Just id
            _ → Nothing

      findAndUpdateContactList = do
            index ← DA.findIndex (findContact userId impersonationId model.experimenting) contacts
            { impersonating } ← contacts !! index
            let updated = DA.modifyAt index (updateHistory { content, id, date }) contacts
            --if impersonating, only the user can start new chats
            case model.experimenting, experimenting, impersonating of
                  Nothing, Just (ImpersonationPayload _), Nothing → Nothing
                  _, _, _ → updated

findContact ∷ Int → Maybe Int → Maybe ExperimentData → Contact → Boolean
findContact userId impersonationId experimenting { user: { id }, impersonating } = userId == id && (DM.isJust experimenting || impersonating == impersonationId)

updateTemporaryId ∷ Array Contact → Int → Int → Int → Array Contact
updateTemporaryId contacts userId previousMessageID messageId = updateContactHistory contacts userId updateTemporary
      where
      updateTemporary history@({ id })
            | id == previousMessageID = history { id = messageId, status = Received }
            | otherwise = history

updateStatus ∷ Array Contact → Int → Array Int → MessageStatus → Array Contact
updateStatus contacts userId ids status = updateContactHistory contacts userId updateSt
      where
      updateSt history@({ id })
            | DA.elem id ids = history { status = status }
            | otherwise = history

markErroredMessage ∷ Array Contact → Int → Int → Array Contact
markErroredMessage contacts userId messageId = updateContactHistory contacts userId updateStatus
      where
      updateStatus history@({ id })
            | messageId == id = history { status = Errored }
            | otherwise = history

--refactor: should be abstract with updateReadCount
updateContactHistory ∷ Array Contact → Int → (HistoryMessage → HistoryMessage) → Array Contact
updateContactHistory contacts userId f = updateContact <$> contacts
      where
      updateContact contact@{ user: { id }, history }
            | id == userId = contact
                    { history = f <$> history
                    }
            | otherwise = contact

markContactUnavailable ∷ Array Contact → Int → Array Contact
markContactUnavailable contacts userId = updateContact <$> contacts
      where
      updateContact contact@{ user: { id } }
            | id == userId = contact
                    { user
                            { availability = Unavailable
                            }
                    }
            | otherwise = contact

checkMissedEvents ∷ ImModel → MoreMessages
checkMissedEvents model@{ experimenting, contacts, user: { id } } =
      model :>
            if DM.isJust experimenting then []
            else
                  [ do
                          let { lastSentMessageID, lastReceivedMessageID } = findLastMessages contacts id

                          if DM.isNothing lastSentMessageID && DM.isNothing lastReceivedMessageID then
                                pure Nothing
                          else
                                CCNT.retryableResponse CheckMissedEvents ResumeMissedEvents (request.im.missedEvents { query: { lastSenderId: lastSentMessageID, lastRecipientId: lastReceivedMessageID } })
                  ]

findLastMessages ∷ Array Contact → Int → { lastSentMessageID ∷ Maybe Int, lastReceivedMessageID ∷ Maybe Int }
findLastMessages contacts sessionUserID =
      { lastSentMessageID: findLast (\h → sessionUserID == h.sender && h.status == Received)
      , lastReceivedMessageID: findLast ((sessionUserID /= _) <<< _.sender)
      }
      where
      findLast f = do
            index ← DA.findLastIndex f allHistories
            { id } ← allHistories !! index
            pure id

      allHistories = DA.sortBy byID <<< DA.concatMap _.history $ DA.filter (DM.isNothing <<< _.impersonating) contacts
      byID { id } { id: anotherID } = compare id anotherID

setName ∷ String → ImModel → NoMessages
setName name model =
      F.noMessages $ model
            { user
                    { name = name
                    }
            }

setAvatar ∷ Maybe String → ImModel → NoMessages
setAvatar base64 model = F.noMessages $ model
      { user
              { avatar = base64
              }
      }

toggleConnectedWebSocket ∷ Boolean → ImModel → MoreMessages
toggleConnectedWebSocket isConnected model@{ hasTriedToConnectYet, errorMessage } =
      model
            { hasTriedToConnectYet = true
            , isWebSocketConnected = isConnected
            , errorMessage = if not isConnected then lostConnectionMessage else if errorMessage == lostConnectionMessage then "" else errorMessage
            } :> if hasTriedToConnectYet && isConnected then [ pure <<< Just $ SpecialRequest CheckMissedEvents ] else [ pure $ Just UpdateDelivered ]
      where
      lostConnectionMessage = "Connection to the server lost. Attempting to automatically reconnect..."

preventStop ∷ Event → ImModel → NextMessage
preventStop event model = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event

checkNotifications ∷ Effect Unit
checkNotifications = do
      status ← CCD.notificationPermission
      when (status == "default") $ FS.send imId ToggleAskNotification

--refactor use popstate subscription
historyChange ∷ Boolean → Effect Unit
historyChange smallScreen = do
      popStateListener ← WET.eventListener $ const handler
      window ← WH.window
      WET.addEventListener popstate popStateListener false $ WHW.toEventTarget window
      where
      handler = do
            CCD.pushState $ routes.im.get {}
            when smallScreen <<< FS.send imId $ ToggleInitialScreen true

setUpWebSocket ∷ Ref CurrentWebSocket → Effect Unit
setUpWebSocket webSocketRef = do
      { webSocket } ← ER.read webSocketRef
      let webSocketTarget = CIW.toEventTarget webSocket
      --a ref is used to track reconnections and ping intervals
      timerIds ← ER.new { reconnectId: Nothing, pingId: Nothing }
      openListener ← WET.eventListener (open timerIds)
      messageListener ← WET.eventListener runMessage
      closeListener ← WET.eventListener (close timerIds)
      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget

      where
      open timerIds _ = do
            { reconnectId } ← ER.read timerIds
            case reconnectId of
                  Nothing → pure unit
                  Just id → do
                        ET.clearTimeout id
                        ER.modify_ (_ { reconnectId = Nothing }) timerIds
            pong true
            newPingId ← ping
            ER.modify_ (_ { pingId = Just newPingId }) timerIds
            FS.send imId $ ToggleConnected true
            askForUpdates

      ping = do
            pingAction --first run of setInterval is after the delay
            ET.setInterval (1000 * 30) pingAction

      pingAction = do
            { webSocket, ponged } ← ER.read webSocketRef
            isFocused ← CCD.documentHasFocus
            if ponged then do
                  pong false
                  FS.send imId $ SendPing isFocused
            else
                  CIW.close webSocket

      runMessage event = do
            let
                  payload = SU.fromRight <<< CME.runExcept <<< FO.readString <<< CIW.data_ <<< SU.fromJust $ CIW.fromEvent event
                  message = SU.fromRight $ SJ.fromJSON payload
            isFocused ← CCD.documentHasFocus
            case message of
                  Pong { status } → do
                        FS.send imId $ DisplayAvailability status
                        pong true
                  Content cnt → FS.send imId $ ReceiveMessage cnt isFocused

      pong whether = ER.modify_ (_ { ponged = whether }) webSocketRef

      askForUpdates = do
            { webSocket } ← ER.read webSocketRef
            CIW.sendPayload webSocket UpdateHash

      close timerIds _ = do
            FS.send imId $ ToggleConnected false
            { reconnectId, pingId } ← ER.read timerIds
            case pingId of
                  Nothing → pure unit
                  Just id → do
                        ET.clearInterval id
                        ER.modify_ (_ { pingId = Nothing }) timerIds
            when (DM.isNothing reconnectId) do
                  milliseconds ← ERD.randomInt 2000 10000
                  id ← ET.setTimeout milliseconds <<< void $ do
                        newWebSocket ← CIW.createWebSocket
                        ER.modify_ (_ { webSocket = newWebSocket }) webSocketRef
                        setUpWebSocket webSocketRef
                  ER.modify_ (_ { reconnectId = Just id }) timerIds

setSmallScreen ∷ ImModel → NoMessages
setSmallScreen model@{ toggleModal } =
      F.noMessages $ model
            { messageEnter = false
            , smallScreen = true
            , toggleModal = case toggleModal of --tutorial is preload server side
                    Tutorial _ → HideUserMenuModal
                    tm → tm
            }
