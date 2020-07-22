module Client.IM.Main where

import Prelude
import Shared.IM.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame (NoMessages)
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Scroll as CISR
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Control.Monad.Except as CME
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as EC
import Effect.Random as ERD
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame (ListUpdate, QuerySelector(..))
import Flame as F
import Flame.External as FE
import Foreign as FO
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.Types (Editor, JSONResponse(..))
import Shared.Unsafe as SU
import Shared.WebSocketOptions (port)
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.Socket.Event.EventTypes (onOpen, onClose, onMessage)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW
import Web.UIEvent.WheelEvent as WUW

foreign import loadEditor :: Effect Editor
foreign import keyHandled_ :: EffectFn2 Editor (EffectFn1 String Unit) Unit

main :: Effect Unit
main = do
      token <- CCS.getItem tokenKey
      channel <- F.resumeMount (QuerySelector ".im") {
            view: SIV.view,
            init: [pure <<< Just $ SetToken token],
            update
      }

      setUpWebSocket channel token

      editor <- loadEditor
      EU.runEffectFn2 keyHandled_ editor $ EU.mkEffectFn1 (SC.send channel <<< DA.singleton <<< BeforeSendMessage)

      CISR.scrollLastMessage
      --receive profile edition changes
      CCD.addCustomEventListener nameChanged (SC.send channel <<< DA.singleton <<< SetName)
      --display settings/profile page
      FE.send [FE.onClick' [ShowUserContextMenu], FE.onFocus [UpdateReadCount]] channel

setUpWebSocket :: Channel (Array IMMessage) -> String -> Effect Unit
setUpWebSocket channel token = do
      webSocket <- WSW.create ("ws://localhost:" <> show port) []
      SC.send channel <<< DA.singleton $ SetWebSocket webSocket

      let webSocketTarget = WSW.toEventTarget webSocket
      --a ref is used to track reconnections
      timerID <- ER.new Nothing
      openListener <- WET.eventListener $ const (WSW.sendString webSocket <<< SJ.toJSON $ Connect token)
      messageListener <- WET.eventListener $ \event -> do
            maybeID <- ER.read timerID
            DM.maybe (pure unit) (\id -> do
                  ET.clearTimeout id
                  ER.write Nothing timerID) maybeID
            let possiblePayload = CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.fromJust "client.im.main" $ WSEM.fromEvent event
            case possiblePayload of
                  Left e -> EC.log $ "bogus payload " <> show (map FO.renderForeignError e)
                  Right payload -> do
                        case SJ.fromJSON payload of
                              Left e -> EC.log $ "bogus payload " <> e
                              Right r -> do
                                    isFocused <- CCD.documentHasFocus
                                    SC.send channel <<< DA.singleton $ ReceiveMessage r isFocused

      closeListener <- WET.eventListener $ \_ -> do
            maybeID <- ER.read timerID
            when (DM.isNothing maybeID) do
                  CCN.alert "Connection to the server lost. Retrying..."
                  milliseconds <- ERD.randomInt 2000 7000
                  id <- ET.setTimeout milliseconds <<< void $ setUpWebSocket channel token
                  ER.write (Just id) timerID

      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget

update :: ListUpdate IMModel IMMessage
update model  =
      case _ of
            --chat
            BeforeSendMessage content -> CIC.startChat model content
            SendMessage content date -> CIC.sendMessage content date model
            ReceiveMessage payload isFocused -> CIC.receiveMessage isFocused model payload
            --contacts
            ResumeChat id -> CICN.resumeChat id model
            MarkAsRead -> CICN.markRead model
            UpdateReadCount -> CICN.markRead model --when the window is focused updated the read status of current chat
            FetchContacts event -> CICN.fetchContacts (SU.fromJust "contacts.update" $ WUW.fromEvent event) model
            DisplayContacts (JSONResponse contacts) -> CICN.displayContacts contacts model
            --history
            CheckScrollTop -> CIH.checkScrollTop model
            FetchHistory shouldFetch -> CIH.fetchHistory shouldFetch model
            DisplayHistory (JSONResponse history) -> CIH.displayHistory history model
            --suggestion
            PreviousSuggestion -> CIS.previousSuggestion model
            NextSuggestion -> CIS.nextSuggestion model
            DisplayMoreSuggestions (JSONResponse suggestions) -> CIS.displayMoreSuggestions suggestions model
            --user menu
            ConfirmLogout -> CIU.confirmLogout model
            Logout confirmed -> CIU.logout model confirmed
            ShowUserContextMenu event -> CIU.showUserContextMenu model event
            ToggleProfileSettings toggle -> CIU.toggleProfileSettings model toggle
            SetModalContents file root (JSONResponse html) -> CIF.nothingNext model $ CIU.loadModal root html file
            SetUserContentMenuVisible toggle -> F.noMessages $ SN.updateModel model $ _ {  userContextMenuVisible = toggle }
            SetWebSocket webSocket -> setWebSocket webSocket model
            SetToken token -> setToken token model
            SetName name -> setName name model

setWebSocket :: WebSocket -> IMModel -> NoMessages
setWebSocket ws = CIF.diff { webSocket: Just $ WS ws }

setToken :: String -> IMModel -> NoMessages
setToken token = CIF.diff { token: Just token }

--REFACTOR: pick between CIF.diff and SN.update*
setName :: String -> IMModel -> NoMessages
setName name model@(IMModel { user }) = F.noMessages <<< SN.updateModel model $ _ {
      user = SN.updateUser user $ _ {
            name = name
      }
}
