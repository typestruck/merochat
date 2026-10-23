module Client.Im.Main (main, application) where

import Prelude

import Client.AppId (imAppId, profileAppId)
import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.Ui.Window as CUW
import Client.Im.Asks as CIA
import Client.Im.Changelog as CICL
import Client.Im.Chat as CICH
import Client.Im.Contacts as CICO
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.History as CIH
import Client.Im.ModalsMenu as CIM
import Client.Im.Posts as CIP
import Client.Im.Praise as CIPR
import Client.Im.Scroll as CISC
import Client.Im.SmallScreen as CISS
import Client.Im.Suggestion as CISU
import Client.Im.WebSocket as CIW
import Client.Im.WebSocket.Connection (WebSocket)
import Client.Im.WebSocket.Events as CIWE
import Client.Location as CL
import Client.Lynx.Room as CLR
import Client.Network (routes)
import Client.Network as CN
import Client.Ui.Event as CUEV
import Data.Array ((:))
import Data.Array as DA
import Data.DateTime (DateTime)
import Data.DateTime as DD
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol as DSY
import Data.Time.Duration (Days(..), Milliseconds(..), Minutes(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Unsafe as EU
import Flame (Update, Application)
import Flame as F
import Flame.Html.Event (Event)
import Flame.Subscription as FS
import Safe.Coerce as SC
import Shared.Backer.Contact (backerId)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Types (FocusEvent(..), For(..), ImMessage(..), ImModel, IntWrapper, PostMode(..), ReactWith(..), RequestFailure, RetryableRequest(..), ShowContextMenu(..), WebSocketConnectionStatus(..), WebSocketPayloadServer(..))
import Shared.Im.View as SIV
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Profile.Mode (ProfileMode(..))
import Shared.Profile.Types as SPT
import Shared.ProfileColumn (ProfileColumn)
import Shared.ResizeInput as SR
import Shared.Routes (routesSpec)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User (ProfileTab(..))
import Shared.User as SUS
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.Event.EventTarget as WEE
import Web.HTML as WH
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW

main ∷ Effect Unit
main = EA.launchAff_ do
      app ← application
      EC.liftEffect $ F.mount imAppId app

application ∷ Aff (Application ImModel ImMessage)
application = do
      --this will need to handle updates too
      user ← CLR.select >>= case _ of
            Nothing → EA.throwError $ EE.error "Merochat cache is invalid. Please report a bug"
            Just user → pure user
      contacts ← DM.fromMaybe [] <$> CLR.select
      suggestions ← DM.fromMaybe [] <$> CLR.select
      webSocketRef ← EC.liftEffect CIWE.createWebSocketState
      lastActiveRef ← EC.liftEffect $ ER.new true

      let
            model ∷ ImModel
            model =
                  { chatting: Nothing
                  , freeToFetchSuggestions: true
                  , temporaryId: 0
                  , loadingContact: Nothing
                  , suggesting: _.id <$> DA.head suggestions
                  , freeToFetchChatHistory: true
                  , react: WithEmoji
                  , suggestionsPage: 1
                  , errorMessage: ""
                  , showSuggestionChatInput: Nothing
                  , lastTyping: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
                  , showBuildProfile: DA.length user.completedFields < 3
                  , smallScreen: true
                  , showMiniChatInput: false
                  , initialScreen: true
                  , temporaryEmail: Nothing
                  , changelogs: []
                  , showChangelogs: false
                  , showCollapsedMiniSuggestions: false
                  , showSuggestionsPostForm: user.totalPosts == 0
                  , temporaryPassword: Nothing
                  , enableNotificationsVisible: false
                  , messageEnter: false
                  , editing: Nothing
                  , showLargeAvatar: false
                  , imageCaption: Nothing
                  , selectedImage: Nothing
                  , webSocketMessages: []
                  , fullContactProfileVisible: false
                  , freeToFetchContactList: true
                  , erroredFields: []
                  , fortune: Nothing
                  , toggleContextMenu: HideContextMenu
                  , modal: if user.completedTutorial then HideModal else Special ShowRules
                  , blockedUsers: []
                  , reportReason: Nothing
                  , collapsedSidebar: false
                  , highlighted: Nothing
                  , privateNote: Nothing
                  , reportComment: Nothing
                  , imUpdated: false
                  , praise:
                          { freeToSave: true
                          , selected: Nothing
                          , other: Nothing
                          , freeToFetch: true
                          }
                  , asks:
                          { freeToFetch: true
                          , freeToSend: true
                          , unallowed: []
                          , sent: []
                          , question: Nothing
                          }
                  , posts:
                          { freeToFetch: true
                          , freeToSend: true
                          , mode: TextOnly
                          , text: Nothing
                          , link: Nothing
                          , image: Nothing
                          , caption: Nothing
                          }
                  , webSocketStatus: Closed
                  , failedRequests: []
                  , modalsLoaded: []
                  , hash: ""
                  , contacts
                  , suggestions
                  , user
                  }

      pure
            { model
            , view: SIV.view true
            , subscribe:
                    [ --FSD.onClick' ToggleUserContextMenu
                    --   --possible workaround for a firefox bug
                    --   , onBeforeUnload $ CloseWebSocket Desktop
                    --   --we need focus blur and visibility change to handle all case of when the window is active or not
                    --   , FSW.onFocus (Refocus FocusBlur)
                    --   , onBlur (Refocus FocusBlur)
                    --   , onVisibilityChange (Refocus VisibilityChange)
                    --   , FSW.onOffline $ CloseWebSocket Always
                    --, FS.onCustomEvent' toQuote (pure <<< Just <<< HighlightMessage)
                    ]
            , update: update { webSocketRef, lastActiveRef }
            }

update ∷ _ → Update ImModel ImMessage
update st model =
      case _ of
            --chat
            DropFile event → CICH.catchFile event model
            ResizeChatInput event → SR.resizeInputFrom event model
            PrepareSelectedImage event → CICH.prepareSelectedImage event model
            EnterSendMessage elementId event → CICH.enterSendMessage elementId event model
            ForceSendMessage elementId → CICH.forceSendMessage elementId model
            SendMessage elementId content dt → CICH.prepareSendMessage elementId content dt webSocket model
            SetSelectedImage selected → CICH.setSelectedImage selected model
            SetEmoji elementId event → CICH.setEmoji elementId event model
            ToggleMessageEnter → CICH.toggleMessageEnter model
            BeforeAudioMessage → CICH.beforeAudioMessage model
            AudioMessage touch → CICH.audioMessage touch model
            ResumeWebSocketMessage payload → CIWE.resumeWebSocketMessage payload webSocket model
            ClearWebSocketMessages → CIWE.clearWebSocketMessages model
            ToggleMiniChatInput → CICH.toggleMiniChatInput model
            SendAudioMessage base64 → CICH.sendAudioMessage base64 model
            FocusInput elementId → focusInput elementId model
            QuoteMessage messageId message et → CICH.quoteMessage messageId message et model
            EditMessage message id → CICH.editMessage message id model
            ToggleSuggestionChatInput id → CISU.toggleSuggestionChatInput id model
            DeleteMessage id → CICH.deleteMessage id webSocket model
            SetTyping text → CICH.sendTyping text (EU.unsafePerformEffect EN.nowDateTime) webSocket model
            ToggleTyping userId tId → F.noMessages $ CICH.toggleTyping userId tId model
            MessageFromExperiment userId message → CICH.messageFromExperiment userId message webSocket model

            --contacts
            ResumeChat userId → CICO.resumeChat userId model
            SetDeliveredStatus → CICO.setDeliveredStatus webSocket model
            SetReadStatus userId → CICO.setReadStatus userId webSocket model
            CheckFetchContacts event → CICO.checkFetchContacts event model
            UpdateDraft userId draft → CICO.updateDraft userId draft model
            SpecialRequest (FetchContacts shouldFetch) → CICO.fetchContacts shouldFetch model
            SpecialRequest (DeleteChat tupleId) → CICO.deleteChat tupleId model
            DisplayContacts contacts → CICO.displayContacts contacts model
            DisplayNewContacts contacts → CICO.displayNewContacts contacts model
            DisplaySuggestionContact userId contacts → CICO.displaySuggestionContact userId contacts model
            DisplayMissedContacts missed → CICO.displayMissedContacts missed model

            --history
            SpecialRequest (FetchHistory userId shouldFetch) → CIH.fetchHistory userId shouldFetch model
            DisplayHistory userId history → CIH.displayHistory userId history model
            SetReactWithText id event → CIH.setReacWithText id event model
            React userId messageId value event → CIH.react userId messageId value event model
            DisplayReaction userId messageId reaction → CIH.displayReaction userId messageId reaction model

            --asks
            SpecialRequest (FetchAsks userId) → CIA.fetchAsks userId model
            SetAsk value → CIA.setAsk value model
            SendAsk userId → CIA.sendAsk userId model
            AfterSendAsk userId allowed → CIA.afterSendAsk userId allowed model
            ToggleShowing userId for ShowAsks → CIA.toggleShowing userId for model
            DisplayAsks userId asks → CIA.displayAsks userId asks model

            --praise
            SpecialRequest (FetchPraise userId) → CIPR.fetchPraise userId model
            ToggleShowing userId for ShowPraise → CIPR.toggleShowing userId for model
            TogglePraise userId praise → CIPR.togglePraise userId praise model
            SetOtherPraise praise → CIPR.setOtherPraise praise model
            SavePraise → CIPR.savePraise model
            AfterSavePraise userId allowed → CIPR.afterSavePraise userId allowed model
            DisplayPraise userId praise → CIPR.displayPraise userId praise model

            --posts
            DisplayPosts userId posts → CIP.displayPosts userId posts model
            SpecialRequest (FetchPosts userId pagination) → CIP.fetchPosts userId pagination model
            ToggleSuggestionPostForm → CIP.togglePostForm model
            SetPostMode mode → CIP.setPostMode mode model
            SetPostText content → CIP.setPostText content model
            SetPostLink content → CIP.setPostLink content model
            PreparePostImage event → CIP.preparePostImage event model
            SetPostImage selected → CIP.setPostImage selected model
            SetPostCaption content → CIP.setPostCaption content model
            SendPost → CIP.sendPost model
            AfterSendPost id → CIP.afterSendPost id webSocket model
            ToggleShowing userId for ShowPosts → CIP.toggleShowing userId ShowPosts for model

            --suggestion
            FetchMoreSuggestions → CISU.fetchMoreSuggestions webSocket model
            ResumeSuggesting → CISU.resumeSuggesting model
            ToggleContactProfile → CISU.toggleContactProfile model
            ToggleLargeAvatar → CISU.toggleLargeAvatar model
            ToggleCollapsedMiniSuggestions → CISU.toggleCollapsedMiniSuggestions model
            RefreshOnlineSuggestions → CISU.refreshOnlineSuggestions webSocket model
            SpecialRequest (Favorite id) → CISU.favorite id model
            SpecialRequest PreviousSuggestion → CISU.previousSuggestion webSocket model
            SpecialRequest NextSuggestion → CISU.nextSuggestion webSocket model
            SpecialRequest (BlockUser id) → blockUser webSocket id model
            DisplayMoreSuggestions suggestions → CISU.displayMoreSuggestions webSocket suggestions model
            ToggleSuggestionsFrom from → CISU.toggleSuggestionsFrom webSocket from model
            ResumeSuggestionChat userId → CISU.resumeSuggestionChat userId model

            --user menu
            ToggleInitialScreen toggle → CIM.toggleInitialScreen toggle model
            Logout after → CIM.logout after model
            ToggleUserContextMenu event → toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) → CIM.modal toggle model
            SetModalContents file root html → CIM.setModalContents file root html model

            --websocket
            ReconnectWebSocket → CIWE.reconnectWebSocket st.webSocketRef model
            TrackAvailability → CIWE.trackAvailability webSocket model
            UpdateWebSocketStatus status → CIWE.updateWebSocketStatus status model
            CloseWebSocket when → CIWE.closeWebSocket when st.webSocketRef model
            ReceiveMessage payload isFocused → CIWE.receiveMessage webSocket isFocused payload model

            --changelogs
            FetchChangelog → CICL.fetchChangelog model
            DisplayChangelog changelogs → CICL.displayChangelog changelogs model
            ToggleChangelog → CICL.toggleChangelog model
            ToggleSidebar → CICO.toggleSidebar model
            PerformChangelogAction action value → CICL.performChangelogAction action value model

            --main
            SetContextMenuToggle toggle → toggleContextMenu toggle model
            ReloadPage → reloadPage model
            RemoveChatBackground → removeChatBackground model
            HideBuildProfile → hideBuildProfile model
            SetUserFromProfile user → storeUser $ model { user = user }
            SetNameFromProfile name → setName name model
            SendUpdatedProfile → sendUpdatedProfile webSocket model
            SetAvatarFromProfile base64 → setAvatar base64 model
            ConnectWebSocket → CIWE.startWebSocket st.webSocketRef model
            SetCompletedFields fields → setCompletedFields fields model
            Escape → escapeFrom model
            ToggleAskNotification → toggleAskNotification model
            CreateUserFromTemporary → registerUser model
            AcknowledgeRules inputted → acknowledgeRules inputted model
            StoreUser → storeUser model
            PreventStop event → preventStop event model
            CheckUserExpiration → checkUserExpiration model
            Refocus e → refocus e st.lastActiveRef webSocket model
            SetChatBackgroundFromProfile toggle url → setChatBackgroundFromProfile toggle url model
            HighlightMessage id → highlightMessage id model
            ClearHighlightedMessage → F.noMessages $ model { highlighted = Nothing }
            TerminateTemporaryUser → terminateAccount model
            SpecialRequest FetchMissedContacts → fetchMissedContacts model
            SetField setter → F.noMessages $ setter model
            SetPrivateNote note → F.noMessages $ model { privateNote = note }
            ToggleFortune isVisible → toggleFortune isVisible model
            ToggleScrollChatDown scroll userId → toggleScrollChatDown scroll userId model
            DisplayFortune sequence → displayFortune sequence model
            RequestFailed failure → handleRequestFailure failure model
            SpecialRequest (ReportUser userId) → report userId webSocket model
            SpecialRequest (SavePrivateNote target note) → savePrivateNote target note model
            AfterSavePrivateNote target note → afterSavePrivateNote target note model
            SetSmallScreen → CISS.setSmallScreen model
            SetRegistered → setRegistered model
            SetPrivacySettings ps → setPrivacySettings ps model
            ToggleShowing userId for ShowInfo → toggleShowing userId ShowInfo for model
      where
      { webSocket } = EU.unsafePerformEffect $ ER.read st.webSocketRef -- u n s a f e

escapeFrom ∷ ImModel → MoreMessages
escapeFrom model = case model.modal of
      HideModal | model.showLargeAvatar → CISU.toggleLargeAvatar model
      HideModal | model.fullContactProfileVisible → CISU.toggleContactProfile model
      HideModal | DM.isJust model.chatting → CISU.resumeSuggesting model
      _ → CIM.modal HideModal model

toggleShowing ∷ Int → ProfileTab → For → ImModel → MoreMessages
toggleShowing userId tab for model =
      case for of
            ForSuggestions → toggleShowingSuggestions userId tab model
            ForContacts → toggleShowingContacts userId tab model

toggleShowingSuggestions ∷ Int → ProfileTab → ImModel → MoreMessages
toggleShowingSuggestions userId toggle model =
      model
            { suggestions = map upd model.suggestions
            --we need this bookkeeping for big suggestion cards
            , suggesting = Just userId
            , modal = Special $ ShowSuggestionCard userId
            } /\ []
      where
      upd suggestion
            | suggestion.id == userId = suggestion { showing = toggle }
            | otherwise = suggestion

toggleShowingContacts ∷ Int → ProfileTab → ImModel → MoreMessages
toggleShowingContacts userId toggle model =
      model
            { contacts = map upd model.contacts
            , fullContactProfileVisible = true
            } /\ []
      where
      upd contact
            | contact.user.id == userId = contact { user = contact.user { showing = toggle } }
            | otherwise = contact

removeChatBackground ∷ ImModel → NoMessages
removeChatBackground model = model { toggleContextMenu = HideContextMenu, user { ownBackground = true } } /\ [ save ]
      where
      save = do
            void <<< CN.silentRequest $ routes.settings.chat.background { body: { ownBackground: true, image: model.user.chatBackground } }
            pure Nothing

setChatBackgroundFromProfile ∷ Boolean → Maybe String → ImModel → NoMessages
setChatBackgroundFromProfile toggle url model = model { user { ownBackground = toggle, chatBackground = url } } /\ []

toggleContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleContextMenu toggle model = F.noMessages model { toggleContextMenu = toggle }

setRegistered ∷ ImModel → NoMessages
setRegistered model = model { user { temporary = false } } /\
      [ pure <<< Just <<< SpecialRequest <<< ToggleModal $ Screen (ShowProfile Edit)
      , do
              EA.delay $ Milliseconds 1000.0
              EC.liftEffect $ FS.send profileAppId SPT.AfterRegistration
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
                  VisibilityChange → CUD.documentIsNotHidden
                  FocusBlur → CUD.documentHasFocus
            if active then action else pure Nothing

      updateLastSeen = EC.liftEffect do
            active ← case focusEvent of
                  VisibilityChange → CUD.documentIsNotHidden
                  FocusBlur → CUD.documentHasFocus
            lastActive ← ER.read lastActiveRef
            when (active /= lastActive) do
                  CIW.sendPayload webSocket $ UpdateAvailability { online: active }
                  ER.write active lastActiveRef
            pure Nothing

registerUser ∷ ImModel → MoreMessages
registerUser model@{ temporaryEmail, temporaryPassword, erroredFields } =
      if invalidEmail then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DSY.reflectSymbol (Proxy ∷ _ "temporaryEmail") }
      else if invalidPassword then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DSY.reflectSymbol (Proxy ∷ _ "temporaryPassword") }
      else
            model { erroredFields = [] } /\
                  [ do
                          status ← CN.formRequest (show TemporaryUserSignUpForm) $ routes.im.register { body: { email: SU.fromJust temporaryEmail, password: SU.fromJust temporaryPassword } }
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
              status ← CN.formRequest (show ConfirmAccountTerminationForm) $ routes.settings.account.terminate { body: {} }
              when (status == Success) do
                    EA.delay $ Milliseconds 3000.0
                    EC.liftEffect <<< CL.setLocation $ routesSpec.login.get {}
              pure Nothing
      ]

checkUserExpiration ∷ ImModel → MoreMessages
checkUserExpiration model@{ user: { temporary, joined } }
      | temporary && SUS.temporaryUserExpiration joined <= Days 1.0 = model /\ [ pure <<< Just <<< SpecialRequest <<< ToggleModal $ Screen (ShowProfile Edit) ]
      | otherwise = F.noMessages model

setPrivacySettings ∷ PrivacySettings → ImModel → NextMessage
setPrivacySettings { readReceipts, typingStatus, profileVisibility, onlineStatus, messageTimestamps, lastMessageOnContactList, suggestionsFrom } model =
      model
            { user
                    { profileVisibility = profileVisibility
                    , readReceipts = readReceipts
                    , typingStatus = typingStatus
                    , onlineStatus = onlineStatus
                    , messageTimestamps = messageTimestamps
                    , suggestionsFrom = suggestionsFrom
                    , lastMessageOnContactList = lastMessageOnContactList
                    }

            } /\ [ pure $ Just FetchMoreSuggestions ]

acknowledgeRules ∷ String → ImModel → NextMessage
acknowledgeRules inputted model =
      if not model.user.completedTutorial && DS.contains (Pattern "i understand") cleaned then
            model { user { completedTutorial = true }, suggestions = DA.filter ((_ /= sender) <<< _.id) model.suggestions } /\ [ hide, completeTutorial, greet ]
      else
            model /\ []
      where
      cleaned = DS.trim $ DS.toLower inputted

      hide = do
            EA.delay $ Milliseconds 400.0
            pure <<< Just <<< SpecialRequest $ ToggleModal HideModal

      completeTutorial = do
            void <<< CN.silentRequest $ routes.im.tutorial {}
            pure $ Just StoreUser

      sender = 4
      greet = do
            EA.delay $ Milliseconds 2000.0
            void <<< CN.silentRequest $ routes.im.greeting {}
            contact ← CN.silentRequest $ routes.im.contact { query: { id: sender } }
            pure <<< Just $ DisplayNewContacts contact

storeUser ∷ ImModel → NoMessages
storeUser model = model /\ [ store ]
      where
      store = do
            CLR.insert model.user
            pure Nothing

blockUser ∷ WebSocket → Int → ImModel → NextMessage
blockUser webSocket id model = updateAfterBlock id model /\ [ block, track ]
      where
      block = do
            result ← CN.request $ routes.im.block { body: { id } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { routes: BlockUser id, errorMessage: Nothing }
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
            result ← CN.request $ routes.im.report { body: { userId, reason: rs, comment: model.reportComment } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { routes: ReportUser userId, errorMessage: Nothing }
                  _ → do
                        EC.liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id: userId }
                        pure Nothing
      track = pure $ Just TrackAvailability

savePrivateNote ∷ Int → Maybe String → ImModel → MoreMessages
savePrivateNote target note model = model { privateNote = Nothing, modal = HideModal } /\ [ save ]
      where
      save = do
            result ← CN.request $ routes.im.note { body: { target, content: note } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { routes: SavePrivateNote target note, errorMessage: Nothing }
                  _ → pure <<< Just $ AfterSavePrivateNote target note

afterSavePrivateNote ∷ Int → Maybe String → ImModel → NoMessages
afterSavePrivateNote target note model =
      model
            { suggestions = map updateSuggestions model.suggestions
            , contacts = map updateContacts model.contacts
            } /\ []
      where
      updateSuggestions s
            | s.id == target = s { privateNote = note }
            | otherwise = s

      updateContacts c
            | c.user.id == target = c { user = c.user { privateNote = note } }
            | otherwise = c

reloadPage ∷ ImModel → NextMessage
reloadPage model = model /\ [ EC.liftEffect CL.reload *> pure Nothing ]

toggleAskNotification ∷ ImModel → NoMessages
toggleAskNotification model =
      model
            { enableNotificationsVisible = not model.enableNotificationsVisible
            } /\ []

toggleUserContextMenu ∷ Event → ImModel → MoreMessages
toggleUserContextMenu event model
      | model.toggleContextMenu /= HideContextMenu =
              F.noMessages model { toggleContextMenu = HideContextMenu }
      | otherwise =
              model /\
                    [
                      --we cant use node.contains as some of the elements are dynamically created/destroyed
                      EC.liftEffect do
                            let
                                  element = SU.fromJust $ do
                                        target ← CUEV.target event
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
                    | elementId == show SuggestionsFilterMenu || parentId == show SuggestionsFilterMenu = ShowSuggestionsFilterMenu
                    | elementId == show CompactProfileContextMenu || parentId == show CompactProfileContextMenu = ShowCompactProfileContextMenu
                    | elementId == show FullProfileContextMenu || parentId == show FullProfileContextMenu = ShowFullProfileContextMenu
                    | elementId == show MiniSuggestionContextMenu || parentId == show MiniSuggestionContextMenu = ShowMiniSuggestionContextMenu
                    | otherwise = HideContextMenu

focusInput ∷ ElementId → ImModel → NextMessage
focusInput elementId model = model /\
      [ do
              element ← EC.liftEffect $ CUD.unsafeGetElementById elementId
              CUE.focus element
              pure Nothing
      ]

handleRequestFailure ∷ RequestFailure → ImModel → NoMessages
handleRequestFailure failure model =
      model
            { failedRequests = failure : model.failedRequests
            , errorMessage = case failure.routes of
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
      | isVisible = model /\ [ Just <<< DisplayFortune <$> CN.silentRequest (routes.im.fortune {}) ]
      | otherwise = F.noMessages $ model
              { fortune = Nothing
              }

toggleScrollChatDown ∷ Boolean → Int → ImModel → MoreMessages
toggleScrollChatDown scroll userId model =
      --avoid unnecessary ui updates since this event could be high frequence (scroll)
      case SIC.findContact userId model.contacts of
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
            CN.retryableRequest FetchMissedContacts DisplayMissedContacts $ routes.im.missedContacts { query: { since, last } }

sinceLastMessage ∷ ImModel → Effect DateTime
sinceLastMessage model = case _.lastMessageDate <$> DA.last model.contacts of
      Nothing → map (SU.fromJust <<< DD.adjust (Minutes (-1.5))) EN.nowDateTime
      Just dt → pure $ SC.coerce dt

setName ∷ String → ImModel → NoMessages
setName name model =
      storeUser $ model
            { user
                    { name = name
                    }
            }

sendUpdatedProfile ∷ WebSocket → ImModel → MoreMessages
sendUpdatedProfile webSocket model = model /\ [ send ]
      where
      send = do
            EC.liftEffect $ CIW.sendPayload webSocket UpdatedProfile
            pure Nothing

setAvatar ∷ Maybe String → ImModel → NoMessages
setAvatar base64 model = storeUser $ model
      { user
              { avatar = base64
              }
      }

preventStop ∷ Event → ImModel → NoMessages
preventStop event model = model /\ [ EC.liftEffect $ CUEV.preventStop event *> pure Nothing ]

--refactor use popstate subscription
historyChange ∷ Boolean → Effect Unit
historyChange smallScreen = do
      popStateListener ← WEE.eventListener $ const handler
      window ← WH.window
      WEE.addEventListener popstate popStateListener false $ WHW.toEventTarget window
      where
      handler = do
            CUW.pushState $ routesSpec.im.get {}
            when smallScreen <<< FS.send imAppId $ ToggleInitialScreen true

hideBuildProfile ∷ ImModel → NoMessages
hideBuildProfile model = model { showBuildProfile = false } /\ []

setCompletedFields ∷ Array ProfileColumn → ImModel → NoMessages
setCompletedFields fields model = storeUser $ model { user = model.user { completedFields = fields } }

highlightMessage ∷ IntWrapper → ImModel → NoMessages
highlightMessage w model = model { highlighted = Just w.id } /\ [ scroll ]
      where
      scroll = do
            EC.liftEffect $ CISC.scrollIntoView { id: w.id }
            EA.delay $ Milliseconds 500.0
            pure $ Just ClearHighlightedMessage
