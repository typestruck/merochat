module Client.IM.Main where

import Prelude
import Shared.IM.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Notification as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Control.Monad.Except as CME
import Data.Array as DA
import Data.Either (Either)
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..))
import Flame as F
import Flame.External as FE
import Foreign as FO
import Partial.Unsafe as UP
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Shared.WebSocketOptions (port)
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Window as WHW
import Web.Socket.Event.EventTypes (onOpen, onClose, onMessage)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

main :: Effect Unit
main = do
      webSocket <- WSW.create ("ws://localhost:" <> show port) []
      --web socket needs to be a ref as any time the connection can closed and recreated by events
      webSocketRef <- ER.new webSocket
      fileReader <- WFR.fileReader
      --used to auth web socket operations
      token <- CCS.getItem tokenKey
      channel <- F.resumeMount (QuerySelector ".im") {
            view: SIV.view,
            init: [],
            update: update { token, fileReader, webSocketRef }
      }

      setUpWebSocket webSocketRef channel token
      --for drag and drop
      CCF.setUpBase64Reader fileReader (DA.singleton <<< ToggleImageForm <<< Just) channel
      --receive profile edition changes
      CCD.addCustomEventListener nameChanged (SC.send channel <<< DA.singleton <<< SetName)
      --display settings/profile page
      FE.send [FE.onClick' [ShowUserContextMenu]] channel
      --image upload
      input <- CIC.getFileInput
      CCF.setUpFileChange (DA.singleton <<< ToggleImageForm <<< Just) input channel

      windowsFocus channel

update :: _ -> ListUpdate IMModel IMMessage
update { webSocketRef, token, fileReader} model  =
      case _ of
            --chat
            --REFACTOR: decide if model should always be first or last parameter
            InsertLink -> CIC.insertLink model
            SetLink link -> CIC.setLink model link
            SetLinkText text -> CIC.setLinkText model text
            ToggleLinkForm -> CIC.toggleLinkForm model
            ToggleEmojisVisible -> CIC.toggleEmojisVisible model
            DropFile event -> CIC.catchFile model fileReader event
            SetUpMessage event -> CIC.setUpMessage model event
            BeforeSendMessage sent content -> CIC.beforeSendMessage model sent content
            SendMessage date -> CIC.sendMessage webSocket token date model
            SetMessageContent cursor content -> CIC.setMessage cursor content model
            ReceiveMessage payload isFocused -> CIC.receiveMessage webSocket token isFocused model payload
            Apply markup -> CIC.applyMarkup markup model
            Preview -> CIC.preview model
            SelectImage -> CIC.selectImage model
            ExitPreview -> CIC.exitPreview model
            ToggleImageForm maybeBase64 -> CIC.toggleImageForm model maybeBase64
            SetImageCaption caption -> CIC.setImageCaption caption model
            ToggleMessageEnter -> CIC.toggleMessageEnter model
            SetEmoji event -> CIC.setEmoji model event
            --contacts
            ResumeChat id -> CICN.resumeChat id model
            MarkAsRead -> CICN.markRead webSocket token model
            UpdateReadCount -> CICN.markRead webSocket token model
            CheckFetchContacts -> CICN.checkFetchContacts model
            FetchContacts shouldFetch -> CICN.fetchContacts shouldFetch model
            DisplayContacts (ContactsPayload contacts) -> CICN.displayContacts contacts model
            --history
            CheckFetchHistory -> CIH.checkFetchHistory model
            FetchHistory shouldFetch -> CIH.fetchHistory shouldFetch model
            DisplayHistory (HistoryPayload history) -> CIH.displayHistory history model
            --suggestion
            PreviousSuggestion -> CIS.previousSuggestion model
            BlockUser id -> CIS.blockUser webSocket token model id
            NextSuggestion -> CIS.nextSuggestion model
            DisplayMoreSuggestions (SuggestionsPayload suggestions) -> CIS.displayMoreSuggestions suggestions model
            --user menu
            ConfirmLogout -> CIU.confirmLogout model
            Logout confirmed -> CIU.logout model confirmed
            ShowUserContextMenu event -> CIU.showUserContextMenu model event
            ToggleProfileSettings toggle -> CIU.toggleProfileSettings model toggle
            SetModalContents file root (ProfileSettingsPayload html) -> CIF.nothingNext model $ CIU.loadModal root html file
            SetUserContentMenuVisible toggle -> F.noMessages $ SN.updateModel model $ _ {  userContextMenuVisible = toggle }
            --main
            SetName name -> setName name model
            PreventStop event -> preventStop model event
      where webSocket = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e
            setName name model@(IMModel { user }) = F.noMessages <<< SN.updateModel model $ _ {
                  user = SN.updateUser user $ _ {
                        name = name
                  }
            }
            preventStop model event = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event

windowsFocus ::  Channel (Array IMMessage) -> Effect Unit
windowsFocus channel = do
      focusListener <- WET.eventListener $ const (SC.send channel $ DA.singleton UpdateReadCount)
      --focus event has to be on the window as chrome is a whiny baby about document
      window <- WH.window
      WET.addEventListener focus focusListener false $ WHW.toEventTarget window

setUpWebSocket :: Ref WebSocket -> Channel (Array IMMessage) -> String -> Effect Unit
setUpWebSocket webSocketRef channel token = do
      webSocket <- ER.read webSocketRef
      let webSocketTarget = WSW.toEventTarget webSocket
      --a ref is used to track reconnections
      timerID <- ER.new Nothing
      openListener <- WET.eventListener $ const (WSW.sendString webSocket <<< SJ.toJSON $ Connect token)
      messageListener <- WET.eventListener $ \event -> do
            maybeID <- ER.read timerID
            DM.maybe (pure unit) (\id -> do
                  ET.clearTimeout id
                  ER.write Nothing timerID) maybeID
            let payload = fromRight' <<< CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.fromJust $ WSEM.fromEvent event
                message = fromRight' $ SJ.fromJSON payload
            isFocused <- CCD.documentHasFocus
            SC.send channel <<< DA.singleton $ ReceiveMessage message isFocused

      closeListener <- WET.eventListener $ \_ -> do
            maybeID <- ER.read timerID
            when (DM.isNothing maybeID) do
                  CCN.alert "Connection to the server lost. Retrying..."
                  milliseconds <- ERD.randomInt 2000 7000
                  id <- ET.setTimeout milliseconds <<< void $ do
                        newWebSocket <- WSW.create ("ws://localhost:" <> show port) []
                        ER.write newWebSocket webSocketRef
                        setUpWebSocket webSocketRef channel token
                  ER.write (Just id) timerID

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget

      where fromRight' :: forall a b. Either a b -> b
            fromRight' et = UP.unsafePartial (DE.fromRight et)
