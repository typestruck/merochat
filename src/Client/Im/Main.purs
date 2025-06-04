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
import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Client.Im.History as CIH
import Client.Im.Notification as CIN
import Client.Im.Pwa as CIP
import Client.Im.SmallScreen as CISS
import Client.Im.Suggestion as CIS
import Client.Im.UserMenu as CIU
import Client.Im.WebSocket as CIW
import Client.Im.WebSocket.Events as CIWE
import Data.Array ((:))
import Data.Array as DA
import Data.DateTime as DDT
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol as DST
import Data.Symbol as TDS
import Data.Time.Duration (Days(..), Milliseconds(..), Minutes(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Now as EN
import Effect.Random as ERN
import Effect.Ref as ER
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Flame.Subscription.Document as FSD
import Flame.Subscription.Window as FSW
import Safe.Coerce as SC
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SCN
import Shared.Im.View as SIV
import Shared.Network (RequestStatus(..))
import Shared.Options.MountPoint (imId, profileId)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Profile.Types as SPT
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User as SUR
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
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

      --im is server side rendered
      F.resumeMount (QuerySelector $ "#" <> show Im) imId
            { view: SIV.view true
            , subscribe:
                    [
                      --display settings/profile/etc page menus
                      FSD.onClick' ToggleUserContextMenu
                    ,
                      --focus event has to be on the window as chrome is a whiny baby about document
                      FSW.onFocus $ SetReadStatus Nothing
                    ]
            , init: [] -- we use subscription instead of init events
            , update: update { webSocketRef }
            }

      smallScreen ← CISS.checkSmallScreen
      pwa ← CIP.checkPwa

      when smallScreen CISS.sendSmallScreen
      when (pwa || not smallScreen) CIN.checkNotifications
      when pwa $ FS.send imId StartPwa

      --disable the back button on desktop/make the back button go back to previous screen on mobile
      CCD.pushState $ routes.im.get {}
      historyChange smallScreen

      --image upload
      input ← CCD.unsafeGetElementById ImageFileInput
      CCF.setUpFileChange (\width height base64 → SetSelectedImage $ Just { width, height, base64 }) input imId

update ∷ _ → ListUpdate ImModel ImMessage
update st model =
      case _ of
            --chat
            ToggleChatModal modal → CIC.toggleModal modal model
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
            SpecialRequest (FetchContacts shouldFetch) → CICN.fetchContacts shouldFetch model
            SpecialRequest (DeleteChat tupleId) → CICN.deleteChat tupleId model
            DisplayContacts contacts → CICN.displayContacts contacts model
            DisplayNewContacts contacts → CICN.displayNewContacts contacts model
            DisplayMissedContacts missed → CICN.resumeMissedEvents missed model

            --history
            SpecialRequest (FetchHistory userId shouldFetch) → CIH.fetchHistory userId shouldFetch model
            DisplayHistory userId history → CIH.displayHistory userId history model

            --suggestion
            FetchMoreSuggestions → CIS.fetchMoreSuggestions model
            ResumeSuggesting → CIS.resumeSuggesting model
            ToggleContactProfile → CIS.toggleContactProfile model
            ToggleCollapsedMiniSuggestions → CIS.toggleCollapsedMiniSuggestions model
            SpecialRequest PreviousSuggestion → CIS.previousSuggestion model
            SpecialRequest NextSuggestion → CIS.nextSuggestion model
            SpecialRequest (BlockUser id) → blockUser webSocket id model
            DisplayMoreSuggestions suggestions → CIS.displayMoreSuggestions suggestions model
            ToggleSuggestionsFromOnline → CIS.toggleSuggestionsFromOnline model
            SetBugging mc → CIS.setBugging mc model

            --user menu
            ToggleInitialScreen toggle → CIU.toggleInitialScreen toggle model
            Logout after → CIU.logout after model
            ToggleUserContextMenu event → toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) → CIU.toggleModal toggle model
            SetModalContents file root html → CIU.setModalContents file root html model

            --main
            StartPwa → CIP.startPwa model
            SetContextMenuToggle toggle → toggleContextMenu toggle model
            ReloadPage → reloadPage model
            ReceiveMessage payload isFocused → CIWE.receiveMessage webSocket isFocused payload model
            SetNameFromProfile name → setName name model
            SetAvatarFromProfile base64 → setAvatar base64 model
            AskNotification → askNotification model
            ToggleAskNotification → toggleAskNotification model
            CreateUserFromTemporary → registerUser model
            PreventStop event → preventStop event model
            CheckUserExpiration → checkUserExpiration model
            FinishTutorial → finishTutorial model
            ToggleConnected isConnected → CIWE.toggleConnectedWebSocket isConnected model
            TerminateTemporaryUser → terminateAccount model
            SpecialRequest FetchMissedContacts → fetchMissedContacts model
            SpecialRequest (WaitFetchMissedContacts n) → waitFetchMissedContacts n model
            SetField setter → F.noMessages $ setter model
            ToggleFortune isVisible → toggleFortune isVisible model
            ToggleScrollChatDown scroll userId → toggleScrollChatDown scroll userId model
            DisplayFortune sequence → displayFortune sequence model
            RequestFailed failure → handleRequestFailure failure model
            SpecialRequest (ReportUser userId) → report userId webSocket model
            SetSmallScreen → CISS.setSmallScreen model
            SendPing isActive → CIWE.sendPing webSocket isActive model
            SetRegistered → setRegistered model
            SetPrivacySettings ps → setPrivacySettings ps model
            DisplayAvailability availability → displayAvailability availability model
      where
      { webSocket } = EU.unsafePerformEffect $ ER.read st.webSocketRef -- u n s a f e

toggleContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleContextMenu toggle model = F.noMessages model { toggleContextMenu = toggle }

displayAvailability ∷ AvailabilityStatus → ImModel → NoMessages
displayAvailability avl model = F.noMessages $ model
      { contacts = map updateContact model.contacts
      , suggestions = map updateUser model.suggestions
      }
      where
      availability = DH.fromArray $ map (\t → Tuple t.id t.status) avl
      updateContact contact = case DH.lookup contact.user.id availability of
            Just status → contact { user { availability = status } }
            Nothing → contact
      updateUser user = case DH.lookup user.id availability of
            Just status → user { availability = status }
            Nothing → user

setRegistered ∷ ImModel → NoMessages
setRegistered model = model { user { temporary = false } } /\
      [ do
              EC.liftEffect $ FS.send profileId SPT.AfterRegistration
              pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile
      ]

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
      | temporary && SUR.temporaryUserExpiration joined <= Days 1.0 = model /\ [ pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile ]
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
finishTutorial model@{ toggleModal } = model { user { completedTutorial = true } } /\ [ finish, greet ]
      where
      sender = 4
      finish = do
            void <<< CCNT.silentResponse $ request.im.tutorial {}
            case toggleModal of
                  Tutorial _ → pure <<< Just <<< SpecialRequest $ ToggleModal HideUserMenuModal
                  _ → pure Nothing
      greet = do
            EA.delay $ Milliseconds 2000.0
            void <<< CCNT.silentResponse $ request.im.greeting {}
            contact ← CCNT.silentResponse $ request.im.contact { query: { id: sender } }
            pure <<< Just $ DisplayNewContacts contact

blockUser ∷ WebSocket → Int → ImModel → NextMessage
blockUser webSocket id model =
      updateAfterBlock id model /\
            [ do
                    result ← CCN.defaultResponse $ request.im.block { body: { id } }
                    case result of
                          Left _ → pure <<< Just $ RequestFailed { request: BlockUser id, errorMessage: Nothing }
                          _ → do
                                EC.liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id }
                                pure Nothing
            ]

updateAfterBlock ∷ Int → ImModel → ImModel
updateAfterBlock blocked model@{ contacts, suggestions, blockedUsers } =
      model
            { contacts = DA.filter ((blocked /= _) <<< fromContact) contacts
            , suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
            , blockedUsers = blocked : blockedUsers
            , chatting = Nothing
            , failedRequests = []
            , initialScreen = true
            , toggleModal = HideUserMenuModal
            , toggleContextMenu = HideContextMenu
            }
      where
      fromContact { user } = fromUser user
      fromUser { id } = id

report ∷ Int → WebSocket → ImModel → MoreMessages
report userId webSocket model@{ reportReason, reportComment } = case reportReason of
      Just rs →
            updateAfterBlock userId
                  ( model
                          { reportReason = Nothing
                          , reportComment = Nothing
                          }
                  ) /\
                  [ do
                          result ← CCN.defaultResponse $ request.im.report { body: { userId, reason: rs, comment: reportComment } }
                          case result of
                                Left _ → pure <<< Just $ RequestFailed { request: ReportUser userId, errorMessage: Nothing }
                                _ → do
                                      EC.liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id: userId }
                                      pure Nothing
                  ]
      Nothing → F.noMessages $ model
            { erroredFields = [ TDS.reflectSymbol (Proxy ∷ Proxy "reportReason") ]
            }

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
                    | elementId == show UserContextMenu || parentId == show UserContextMenu = ShowUserContextMenu
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

handleRequestFailure ∷ RequestFailure → ImModel → MoreMessages
handleRequestFailure failure model =
      model
            { failedRequests = failure : model.failedRequests
            , errorMessage = case failure.request of
                    BlockUser _ → "Could not block user. Please try again"
                    ReportUser _ → "Could not report user. Please try again"
                    PreviousSuggestion → suggestionsError
                    NextSuggestion → suggestionsError
                    WaitFetchMissedContacts 5 → "Cannot sync contacts. Please reload the page"
                    _ → model.errorMessage
            } /\ messages
      where
      suggestionsError = "Could not fetch suggestions. Please try again"

      messages = case failure.request of
            WaitFetchMissedContacts n |  n < 5 -> [pure <<< Just <<< SpecialRequest $ WaitFetchMissedContacts n]
            _ -> []

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
            CCNT.retryableResponse (WaitFetchMissedContacts 1) DisplayMissedContacts $ request.im.missedContacts { query: { since } }

waitFetchMissedContacts ∷ Int → ImModel → MoreMessages
waitFetchMissedContacts n model = model /\ [ fetchIt ]
      where
      fetchIt = do
            since ← EC.liftEffect $ sinceLastMessage model
            ms ← EC.liftEffect $ ERN.randomInt 200 500
            EA.delay <<< Milliseconds <<< DI.toNumber $ n * ms
            CCNT.retryableResponse (WaitFetchMissedContacts $ n + 1) DisplayMissedContacts $ request.im.missedContacts { query: { since } }

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
setAvatar base64 model = F.noMessages $ model
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

