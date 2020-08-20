module Client.IM.Main where

import Prelude


import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Client.IM.WebSocket (WebSocket, onClose, onMessage, onOpen)
import Client.IM.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array as DA
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DD
import Data.Either (Either)
import Data.Either (fromRight) as DE
import Data.Enum (class BoundedEnum)
import Data.Enum (toEnum) as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Class (liftEffect)
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
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.Types
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Window as WHW

main :: Effect Unit
main = do
      webSocket <- CIW.createWebSocket
      --web socket needs to be a ref as any time the connection can closed and recreated by events
      webSocketRef <- ER.new webSocket
      fileReader <- WFR.fileReader
      channel <- F.resumeMount (QuerySelector ".im") {
            view: SIV.view,
            init: [],
            update: update { fileReader, webSocketRef }
      }

      setUpWebSocket webSocketRef channel
      --for drag and drop
      CCF.setUpBase64Reader fileReader (DA.singleton <<< ToggleImageForm <<< Just) channel
      --receive profile edition changes
      CCD.addCustomEventListener nameChanged (SC.send channel <<< DA.singleton <<< SetNameFromProfile)
      --display settings/profile page
      FE.send [FE.onClick' [ShowUserContextMenu]] channel
      --image upload
      input <- CIC.getFileInput
      CCF.setUpFileChange (DA.singleton <<< ToggleImageForm <<< Just) input channel

      windowsFocus channel

update :: _ -> ListUpdate IMModel IMMessage
update { webSocketRef, fileReader} model  =
      case _ of
            --chat
            InsertLink -> CIC.insertLink model
            SetLink link -> CIC.setLink link model
            SetLinkText text -> CIC.setLinkText text model
            ToggleLinkForm -> CIC.toggleLinkForm model
            ToggleEmojisVisible -> CIC.toggleEmojisVisible model
            DropFile event -> CIC.catchFile fileReader event model
            SetUpMessage event -> CIC.setUpMessage event model
            BeforeSendMessage sent content -> CIC.beforeSendMessage sent content model
            SendMessage date -> CIC.sendMessage webSocket date model
            SetMessageContent cursor content -> CIC.setMessage cursor content model
            ReceiveMessage payload isFocused -> CIC.receiveMessage webSocket isFocused payload model
            Apply markup -> CIC.applyMarkup markup model
            Preview -> CIC.preview model
            SelectImage -> CIC.selectImage model
            ExitPreview -> CIC.exitPreview model
            ToggleImageForm maybeBase64 -> CIC.toggleImageForm maybeBase64 model
            SetImageCaption caption -> CIC.setImageCaption caption model
            ToggleMessageEnter -> CIC.toggleMessageEnter model
            SetEmoji event -> CIC.setEmoji event model
            --contacts
            ResumeChat id -> CICN.resumeChat id model
            MarkAsRead -> CICN.markRead webSocket model
            UpdateReadCount -> CICN.markRead webSocket model
            CheckFetchContacts -> CICN.checkFetchContacts model
            FetchContacts shouldFetch -> CICN.fetchContacts shouldFetch model
            DisplayContacts contacts -> CICN.displayContacts contacts model
            DisplayMissedMessages (MissedMessagesPayload contacts) -> CICN.displayMissedMessages contacts model
            --history
            CheckFetchHistory -> CIH.checkFetchHistory model
            FetchHistory shouldFetch -> CIH.fetchHistory shouldFetch model
            DisplayHistory history -> CIH.displayHistory history model
            --suggestion
            PreviousSuggestion -> CIS.previousSuggestion model
            BlockUser id -> CIS.blockUser webSocket id model
            NextSuggestion -> CIS.nextSuggestion model
            DisplayMoreSuggestions (SuggestionsPayload suggestions) -> CIS.displayMoreSuggestions suggestions model
            --user menu
            ConfirmLogout -> CIU.confirmLogout model
            Logout confirmed -> CIU.logout confirmed model
            ShowUserContextMenu event -> CIU.showUserContextMenu event model
            ToggleProfileSettings toggle -> CIU.toggleProfileSettings toggle model
            --REFACTOR: move to the apt file
            SetModalContents file root (ProfileSettingsPayload html) -> CIF.nothingNext model $ CIU.loadModal root html file
            SetUserContentMenuVisible toggle -> F.noMessages $ SN.updateModel model $ _ {  userContextMenuVisible = toggle }
            --main
            SetNameFromProfile name -> setName name model
            PreventStop event -> preventStop event model
            ToggleOnline -> toggleOnline model
            CheckMissedMessages -> checkMissedMessages model
      where webSocket = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e
            setName name model@(IMModel { user }) = F.noMessages <<< SN.updateModel model $ _ {
                  user = user {
                        name = name
                  }
            }
            toggleOnline model@(IMModel { isOnline }) = F.noMessages <<< SN.updateModel model $ _ {
                  isOnline = not isOnline
            }
            preventStop event model = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event
            checkMissedMessages model@(IMModel { contacts }) =
                  model :> [ Just <<< DisplayMissedMessages <$> CCNT.get' (MissedMessages {
                              since: DM.fromMaybe (DateTime (DD.canonicalDate (toEnum' 1970) (toEnum' 1) (toEnum' 1)) $ Time (toEnum' 0) (toEnum' 0) (toEnum' 0) (toEnum' 0)) $ do
                                    { history } <- DA.head contacts
                                    { date: MDateTime dt }  <- DA.head history
                                    pure dt
                            })]
            toEnum' :: forall a. BoundedEnum a => Int -> a
            toEnum' = SU.fromJust <<< DE.toEnum

windowsFocus ::  Channel (Array IMMessage) -> Effect Unit
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
            sendChannel ToggleOnline
            )
      messageListener <- WET.eventListener $ \event -> do
            maybeID <- ER.read timerID
            DM.maybe (pure unit) (\id -> do
                  ET.clearTimeout id
                  ER.write Nothing timerID) maybeID
            let payload = fromRight' <<< CME.runExcept <<< FO.readString <<< CIW.data_ <<< SU.fromJust $ CIW.fromEvent event
                message = fromRight' $ SJ.fromJSON payload
            isFocused <- CCD.documentHasFocus
            sendChannel $ ReceiveMessage message isFocused

      closeListener <- WET.eventListener $ \_ -> do
            sendChannel ToggleOnline
            maybeID <- ER.read timerID
            when (DM.isNothing maybeID) do
                  CCN.alert "Connection to the server lost. Retrying..."
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
      where fromRight' :: forall a b. Either a b -> b
            fromRight' et = UP.unsafePartial (DE.fromRight et)
            sendChannel = SC.send channel <<< DA.singleton
