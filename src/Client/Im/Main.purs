module Client.Im.Main where

import Debug
import Prelude
import Shared.Availability
import Shared.Im.Types
import Shared.User

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Common.Network as CNN
import Client.Im.Chat as CIC
import Client.Im.Contacts as CICN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.History as CIH
import Client.Im.ModalsMenu as CIU
import Client.Im.Notification as CIN
import Client.Im.Pwa as CIP
import Client.Im.SmallScreen as CISS
import Client.Im.Suggestion as CIS
import Client.Im.Theme as CIT
import Client.Im.WebSocket as CIW
import Client.Im.WebSocket.Events as CIWE
import Data.Array ((:))
import Data.Array as DA
import Data.DateTime as DDT
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol as DST
import Data.Time.Duration (Days(..), Milliseconds(..), Minutes(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Unsafe as EU
import Flame (Update, Subscription)
import Flame as F
import Flame.Subscription as FS
import Flame.Subscription.Document as FSD
import Flame.Subscription.Internal.Create as FSIC
import Flame.Subscription.Window as FSW
import Flame.Types (Source(..))
import Safe.Coerce as SC
import Shared.Backer.Contact (backerId)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SCN
import Shared.Im.View as SIV
import Shared.Modal.Types (Modal(..), ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Options.MountPoint (imId, profileId)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Profile.Types as SPT
import Shared.ProfileColumn (ProfileColumn)
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User as SUR
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML as WH
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW
import Web.Socket.WebSocket (WebSocket)

main ∷ Effect Unit
main = do
      webSocketRef ← CIWE.startWebSocket
      lastActiveRef ← ER.new true
      --im is server side rendered
      model ← F.resumeMount (QuerySelector $ "#" <> show Im) imId
            { view: SIV.view true
            , subscribe:
                    [ FSD.onClick' ToggleUserContextMenu
                    --possible workaround for a firefox bug
                    , onBeforeUnload $ CloseWebSocket Desktop
                    --we need focus blur and visibility change to handle all case of when the window is active or not
                    , FSW.onFocus (Refocus FocusBlur)
                    , onBlur (Refocus FocusBlur)
                    , onVisibilityChange (Refocus VisibilityChange)
                    , FSW.onOffline $ CloseWebSocket Always
                    ]
            , update: update { webSocketRef, lastActiveRef }
            }

      smallScreen ← CISS.checkSmallScreen
      pwa ← CIP.checkPwa

      when smallScreen CISS.sendSmallScreen
      when (pwa || not smallScreen) CIN.checkNotifications
      when pwa $ FS.send imId StartPwa

      --when the pwa is opened from a chat notification
      resumeFromNotification

      --disable the back button on desktop/make the back button go back to previous screen on mobile
      CCD.pushState $ routes.im.get {}
      historyChange smallScreen

      --image upload
      input ← CCD.unsafeGetElementById ImageFileInput
      CCF.setUpFileChange (\width height base64 → SetSelectedImage $ Just { width, height, base64 }) input imId

      --greet new users after they have created an account
      unless (model.user.completedTutorial) $ FS.send imId FinishTutorial

update ∷ _ → Update ImModel ImMessage
update st model =
      case _ of
            --chat
            DropFile event → CIC.catchFile event model
            ResizeChatInput event → CIC.resizeChatInput event model
            EnterSendMessage elementId event → CIC.enterSendMessage elementId event model
            ForceSendMessage elementId → CIC.forceSendMessage elementId model
            SendMessage elementId content dt → CIC.prepareSendMessage elementId content dt webSocket model
            SetSelectedImage selected → CIC.setSelectedImage selected model
            SetEmoji elementId event → CIC.setEmoji elementId event model
            ToggleMessageEnter → CIC.toggleMessageEnter model
            BeforeAudioMessage → CIC.beforeAudioMessage model
            AudioMessage touch → CIC.audioMessage touch model
            ResumeWebSocketMessage payload → CIWE.resumeWebSocketMessage payload webSocket model
            ClearWebSocketMessages → CIWE.clearWebSocketMessages model
            ToggleMiniChatInput → CIC.toggleMiniChatInput model
            SendAudioMessage base64 → CIC.sendAudioMessage base64 model
            FocusInput elementId → focusInput elementId model
            QuoteMessage message et → CIC.quoteMessage message et model
            EditMessage message id → CIC.editMessage message id model
            ToggleSuggestionChatInput id → CIS.toggleSuggestionChatInput id model
            DeleteMessage id → CIC.deleteMessage id webSocket model
            SetTyping text → CIC.sendTyping text (EU.unsafePerformEffect EN.nowDateTime) webSocket model
            NoTyping id → F.noMessages $ CIC.toggleTyping id false model
            TypingId id → F.noMessages model { typingIds = DA.snoc model.typingIds $ SC.coerce id }

            --contacts
            ResumeChat userId → CICN.resumeChat userId model
            SetDeliveredStatus → CICN.setDeliveredStatus webSocket model
            SetReadStatus userId → CICN.setReadStatus userId webSocket model
            CheckFetchContacts event → CICN.checkFetchContacts event model
            UpdateDraft userId draft → CICN.updateDraft userId draft model
            SpecialRequest (FetchContacts shouldFetch) → CICN.fetchContacts shouldFetch model
            SpecialRequest (DeleteChat tupleId) → CICN.deleteChat tupleId model
            DisplayContacts contacts → CICN.displayContacts contacts model
            DisplayNewContacts contacts → CICN.displayNewContacts contacts model
            DisplayMissedContacts missed → CICN.displayMissedContacts missed model

            --history
            SpecialRequest (FetchHistory userId shouldFetch) → CIH.fetchHistory userId shouldFetch model
            DisplayHistory userId history → CIH.displayHistory userId history model

            --suggestion
            FetchMoreSuggestions → CIS.fetchMoreSuggestions model
            ResumeSuggesting → CIS.resumeSuggesting model
            ToggleContactProfile → CIS.toggleContactProfile model
            ToggleLargeAvatar → CIS.toggleLargeAvatar model
            ToggleCollapsedMiniSuggestions → CIS.toggleCollapsedMiniSuggestions model
            SpecialRequest PreviousSuggestion → CIS.previousSuggestion model
            SpecialRequest NextSuggestion → CIS.nextSuggestion model
            SpecialRequest (BlockUser id) → blockUser webSocket id model
            DisplayMoreSuggestions suggestions → CIS.displayMoreSuggestions suggestions model
            ToggleSuggestionsFromOnline → CIS.toggleSuggestionsFromOnline model

            --user menu
            ToggleInitialScreen toggle → CIU.toggleInitialScreen toggle model
            Logout after → CIU.logout after model
            ToggleUserContextMenu event → toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) → CIU.modal toggle model
            SetModalContents file root html → CIU.setModalContents file root html model

            --main
            ReconnectWebSocket → CIWE.reconnectWebSocket st.webSocketRef model
            StartPwa → CIP.startPwa model
            SetContextMenuToggle toggle → toggleContextMenu toggle model
            ReloadPage → reloadPage model
            ReceiveMessage payload isFocused → CIWE.receiveMessage webSocket isFocused payload model
            PushedMessages payload → CIP.receiveMessageFromPush payload model
            HideBuildProfile → hideBuildProfile model
            SetNameFromProfile name → setName name model
            SetAvatarFromProfile base64 → setAvatar base64 model
            AskNotification → askNotification model
            SetCompletedFields fields → setCompletedFields fields model
            ToggleAskNotification → toggleAskNotification model
            CreateUserFromTemporary → registerUser model
            FinishTutorial → finishTutorial model
            PreventStop event → preventStop event model
            CheckUserExpiration → checkUserExpiration model
            TrackAvailability → CIWE.trackAvailability webSocket model
            Refocus e → refocus e st.lastActiveRef webSocket model
            UpdateWebSocketStatus status → CIWE.updateWebSocketStatus status model
            CloseWebSocket when → CIWE.closeWebSocket when st.webSocketRef model
            SetTheme theme → CIT.setTheme theme model
            TerminateTemporaryUser → terminateAccount model
            SpecialRequest FetchMissedContacts → fetchMissedContacts model
            SetField setter → F.noMessages $ setter model
            ToggleFortune isVisible → toggleFortune isVisible model
            ToggleScrollChatDown scroll userId → toggleScrollChatDown scroll userId model
            DisplayFortune sequence → displayFortune sequence model
            RequestFailed failure → handleRequestFailure failure model
            SpecialRequest (ReportUser userId) → report userId webSocket model
            SetSmallScreen → CISS.setSmallScreen model
            SetRegistered → setRegistered model
            SetPrivacySettings ps → setPrivacySettings ps model
      where
      { webSocket } = EU.unsafePerformEffect $ ER.read st.webSocketRef -- u n s a f e

resumeFromNotification ∷ Effect Unit
resumeFromNotification = do
      raw ← CCL.queryParameter "resume"
      case raw >>= DI.fromString of
            Just userId → FS.send imId $ ResumeChat userId
            _ → pure unit

toggleContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleContextMenu toggle model = F.noMessages model { toggleContextMenu = toggle }

setRegistered ∷ ImModel → NoMessages
setRegistered model = model { user { temporary = false } } /\
      [ pure <<< Just <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile
      , do
              EA.delay $ Milliseconds 1000.0
              EC.liftEffect $ FS.send profileId SPT.AfterRegistration
              pure Nothing
      ]

refocus ∷ FocusEvent → Ref Boolean → WebSocket → ImModel → MoreMessages
refocus focusEvent lastActiveRef webSocket model =
      if model.webSocketStatus /= Connected then
            model /\ [ whenActive (pure <<< Just $ UpdateWebSocketStatus Reconnect) ]
      else
            model /\ [ updateLastSeen, whenActive (pure <<< Just $ SetReadStatus Nothing) ]
      where
      whenActive action = EC.liftEffect do
            active ← case focusEvent of
                  VisibilityChange → CCD.documentIsNotHidden
                  FocusBlur → CCD.documentHasFocus
            if active then action else pure Nothing

      updateLastSeen = EC.liftEffect do
            active ← case focusEvent of
                  VisibilityChange → CCD.documentIsNotHidden
                  FocusBlur → CCD.documentHasFocus
            lastActive ← ER.read lastActiveRef
            when (active /= lastActive) do
                  CIW.sendPayload webSocket $ UpdateAvailability { online: active }
                  ER.write active lastActiveRef
            pure Nothing

registerUser ∷ ImModel → MoreMessages
registerUser model@{ temporaryEmail, temporaryPassword, erroredFields } =
      if invalidEmail then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol (Proxy ∷ _ "temporaryEmail") }
      else if invalidPassword then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol (Proxy ∷ _ "temporaryPassword") }
      else
            model { erroredFields = [] } /\
                  [ do
                          status ← CCN.formRequest (show TemporaryUserSignUpForm) $ request.im.register { body: { email: SU.fromJust temporaryEmail, password: SU.fromJust temporaryPassword } }
                          case status of
                                Failure _ → pure Nothing
                                Success → pure $ Just SetRegistered
                  ]
      where
      invalidEmail = DM.maybe true (\email → DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email)) temporaryEmail
      invalidPassword = DM.maybe true (\password → DS.length password < passwordMinCharacters) temporaryPassword

terminateAccount ∷ ImModel → NextMessage
terminateAccount model = model /\
      [ do
              status ← CNN.formRequest (show ConfirmAccountTerminationForm) $ request.settings.account.terminate { body: {} }
              when (status == Success) do
                    EA.delay $ Milliseconds 3000.0
                    EC.liftEffect <<< CCL.setLocation $ routes.login.get {}
              pure Nothing
      ]

checkUserExpiration ∷ ImModel → MoreMessages
checkUserExpiration model@{ user: { temporary, joined } }
      | temporary && SUR.temporaryUserExpiration joined <= Days 1.0 = model /\ [ pure <<< Just <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile ]
      | otherwise = F.noMessages model

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
            } /\ [ pure $ Just FetchMoreSuggestions ]

finishTutorial ∷ ImModel → NextMessage
finishTutorial model = model { user { completedTutorial = true } } /\ [ greet ]
      where
      sender = 4
      greet = do
            void <<< CCNT.silentResponse $ request.im.tutorial {}
            EA.delay $ Milliseconds 2000.0
            void <<< CCNT.silentResponse $ request.im.greeting {}
            contact ← CCNT.silentResponse $ request.im.contact { query: { id: sender } }
            pure <<< Just $ DisplayNewContacts contact

blockUser ∷ WebSocket → Int → ImModel → NextMessage
blockUser webSocket id model = updateAfterBlock id model /\ [ block, track ]
      where
      block = do
            result ← CCN.defaultResponse $ request.im.block { body: { id } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { request: BlockUser id, errorMessage: Nothing }
                  _ → do
                        EC.liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id }
                        pure Nothing
      track = pure $ Just TrackAvailability

updateAfterBlock ∷ Int → ImModel → ImModel
updateAfterBlock blocked model@{ contacts, suggestions, blockedUsers } =
      model
            { contacts = DA.filter ((blocked /= _) <<< fromContact) contacts
            , suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
            , blockedUsers = blocked : blockedUsers
            , chatting = Nothing
            , failedRequests = []
            , initialScreen = true
            , modal = HideModal
            , toggleContextMenu = HideContextMenu
            }
      where
      fromContact { user } = fromUser user
      fromUser { id } = id

report ∷ Int → WebSocket → ImModel → MoreMessages
report userId webSocket model = case model.reportReason of
      Just rs →
            updateAfterBlock userId
                  ( model
                          { reportReason = Nothing
                          , reportComment = Nothing
                          }
                  ) /\ [ reportIt rs, track ]
      Nothing → F.noMessages model
      where
      reportIt rs = do
            result ← CCN.defaultResponse $ request.im.report { body: { userId, reason: rs, comment: model.reportComment } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { request: ReportUser userId, errorMessage: Nothing }
                  _ → do
                        EC.liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id: userId }
                        pure Nothing
      track = pure $ Just TrackAvailability

reloadPage ∷ ImModel → NextMessage
reloadPage model = model /\ [ EC.liftEffect CCL.reload *> pure Nothing ]

askNotification ∷ ImModel → MoreMessages
askNotification model = model { enableNotificationsVisible = false } /\ [ EC.liftEffect CCD.requestNotificationPermission *> pure Nothing ]

--refactor: all messages like this can be dryed into a single function
toggleAskNotification ∷ ImModel → NoMessages
toggleAskNotification model@{ enableNotificationsVisible } = F.noMessages $ model
      { enableNotificationsVisible = not enableNotificationsVisible
      }

toggleUserContextMenu ∷ Event → ImModel → MoreMessages
toggleUserContextMenu event model
      | model.toggleContextMenu /= HideContextMenu =
              F.noMessages $ model { toggleContextMenu = HideContextMenu }
      | otherwise =
              model /\
                    [
                      --we cant use node.contains as some of the elements are dynamically created/destroyed
                      EC.liftEffect do
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
                    | elementId == show SuggestionContextMenu || parentId == show SuggestionContextMenu = ShowSuggestionContextMenu
                    | elementId == show CompactProfileContextMenu || parentId == show CompactProfileContextMenu = ShowCompactProfileContextMenu
                    | elementId == show FullProfileContextMenu || parentId == show FullProfileContextMenu = ShowFullProfileContextMenu
                    | elementId == show MiniSuggestionContextMenu || parentId == show MiniSuggestionContextMenu = ShowMiniSuggestionContextMenu
                    | otherwise = HideContextMenu

focusInput ∷ ElementId → ImModel → NextMessage
focusInput elementId model = model /\
      [ EC.liftEffect do
              element ← CCD.getElementById elementId
              WHHE.focus $ SU.fromJust do
                    e ← element
                    WHHE.fromElement e
              pure Nothing
      ]

handleRequestFailure ∷ RequestFailure → ImModel → NoMessages
handleRequestFailure failure model =
      model
            { failedRequests = failure : model.failedRequests
            , errorMessage = case failure.request of
                    BlockUser _ → "Could not block user. Please try again"
                    ReportUser _ → "Could not report user. Please try again"
                    PreviousSuggestion → suggestionsError
                    NextSuggestion → suggestionsError
                    FetchMissedContacts → "Cannot sync contacts. Please reload the page"
                    _ → model.errorMessage
            } /\ []
      where
      suggestionsError = "Could not fetch suggestions. Please try again"

toggleFortune ∷ Boolean → ImModel → MoreMessages
toggleFortune isVisible model
      | isVisible = model /\ [ Just <<< DisplayFortune <$> CCNT.silentResponse (request.im.fortune {}) ]
      | otherwise = F.noMessages $ model
              { fortune = Nothing
              }

toggleScrollChatDown ∷ Boolean → Int → ImModel → MoreMessages
toggleScrollChatDown scroll userId model =
      --avoid unnecessary ui updates since this event could be high frequence (scroll)
      case SCN.findContact userId model.contacts of
            Just contact | contact.scrollChatDown /= scroll → F.noMessages model { contacts = map upd model.contacts }
            _ → F.noMessages model
      where
      upd contact
            | contact.user.id == userId = contact { scrollChatDown = scroll }
            | otherwise = contact

displayFortune ∷ String → ImModel → NoMessages
displayFortune sequence model = F.noMessages $ model
      { fortune = Just sequence
      }

fetchMissedContacts ∷ ImModel → MoreMessages
fetchMissedContacts model = model /\ [ fetchIt ]
      where
      fetchIt = do
            since ← EC.liftEffect $ sinceLastMessage model
            let last = (map _.history <<< DA.last <<< DA.sortWith _.lastMessageDate $ DA.filter ((backerId /= _) <<< _.id <<< _.user) model.contacts) >>= (map _.id <<< DA.last)
            CCNT.retryableResponse FetchMissedContacts DisplayMissedContacts $ request.im.missedContacts { query: { since, last } }

sinceLastMessage ∷ ImModel → Effect DateTimeWrapper
sinceLastMessage model = case _.lastMessageDate <$> DA.last model.contacts of
      Nothing → map (DateTimeWrapper <<< SU.fromJust <<< DDT.adjust (Minutes (-1.5))) EN.nowDateTime
      Just dt → pure dt

setName ∷ String → ImModel → NoMessages
setName name model =
      F.noMessages model
            { user
                    { name = name
                    }
            }

setAvatar ∷ Maybe String → ImModel → NoMessages
setAvatar base64 model = F.noMessages model
      { user
              { avatar = base64
              }
      }

preventStop ∷ Event → ImModel → NoMessages
preventStop event model = model /\ [ EC.liftEffect $ CCD.preventStop event *> pure Nothing ]

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

onVisibilityChange ∷ ∀ message. message → Subscription message
onVisibilityChange = FSIC.createSubscription Document "visibilitychange"

onBlur ∷ ∀ message. message → Subscription message
onBlur = FSIC.createSubscription Window "blur"

onBeforeUnload ∷ ∀ message. message → Subscription message
onBeforeUnload = FSIC.createSubscription Window "beforeunload"

hideBuildProfile ∷ ImModel → NoMessages
hideBuildProfile model = model { showBuildProfile = false } /\ []

setCompletedFields ∷ Array ProfileColumn → ImModel → NoMessages
setCompletedFields fields model = model { user = model.user { completedFields = fields } } /\ []