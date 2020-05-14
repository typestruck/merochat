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
import Client.IM.Scroll as CISR
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Control.Monad.Except as CME
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
import Flame (QuerySelector(..))
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Flame.External as FE
import Foreign as FO
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.Types (Editor)
import Shared.Unsafe as SU
import Shared.WebSocketOptions (port)
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.Socket.Event.EventTypes (onOpen, onClose, onMessage)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

foreign import loadEditor :: Effect Editor
foreign import keyHandled_ :: EffectFn2 Editor (EffectFn1 String Unit) Unit

main :: Effect Unit
main = do
        channel <- FAE.resumeMount (QuerySelector ".im") {
                view: SIV.view,
                init: Nothing,
                update
        }

        token <- CCS.getItem tokenKey
        SC.send channel <<< Just <<< MM $ SetToken token

        setUpWebSocket channel token

        editor <- loadEditor
        EU.runEffectFn2 keyHandled_ editor $ EU.mkEffectFn1 (SC.send channel <<< Just <<< CM <<< SendMessage)

        CISR.scrollLastMessage
        --receive profile edition changes
        CCD.addCustomEventListener nameChanged (SC.send channel <<< Just <<< MM <<< SetName)

        FE.send [FE.onClick' (Just (UMM <<< ShowUserContextMenu))] channel

setUpWebSocket :: Channel (Maybe IMMessage) -> String -> Effect Unit
setUpWebSocket channel token = do
        webSocket <- WSW.create ("ws://localhost:" <> show port) []
        SC.send channel <<< Just <<< MM $ SetWebSocket webSocket

        let webSocketTarget = WSW.toEventTarget webSocket
        --a ref is used to track reconnections
        timerID <- ER.new Nothing
        openListener <- WET.eventListener $ const (WSW.sendString webSocket <<< SJ.toJSON $ Connect token)
        messageListener <- WET.eventListener $ \event -> do
                maybeID <- ER.read timerID
                DM.maybe (pure unit) (\id -> do
                        ET.clearTimeout id
                        ER.write Nothing timerID) maybeID

                let possiblePayload = CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.unsafeFromJust "client.im.main" $ WSEM.fromEvent event
                case possiblePayload of
                        Left e -> EC.log ("bogus payload " <> show (map FO.renderForeignError e))
                        Right payload -> do
                                case SJ.fromJSON payload of
                                        Left e -> EC.log $ "bogus payload " <> show e
                                        Right r -> SC.send channel <<< Just <<< CM $ ReceiveMessage r

        closeListener <- WET.eventListener $ \_ -> do
                maybeID <- ER.read timerID
                when (DM.isNothing maybeID) $ do
                        CCN.alert "Connection to the server lost. Retrying..."
                        milliseconds <- ERD.randomInt 2000 7000
                        id <- ET.setTimeout milliseconds <<< void $ setUpWebSocket channel token
                        ER.write (Just id) timerID

        WET.addEventListener onMessage messageListener false webSocketTarget
        WET.addEventListener onOpen openListener false webSocketTarget
        WET.addEventListener onClose closeListener false webSocketTarget

update :: AffUpdate IMModel IMMessage
update environment@{ message, model, display } =
        case message of
                CM msg -> const <$> CIC.update (environment { message = msg })
                SM msg -> CIS.update $ environment { message = msg }
                CNM msg -> const <$> CICN.update (environment { message = msg })
                UMM msg -> CIU.update $ environment { message = msg }
                MM msg -> set msg

        where   set = case _ of
                        SetWebSocket webSocket -> setWebSocket webSocket
                        SetToken token -> setToken token
                        SetName name -> setName name

setWebSocket :: WebSocket -> Aff (IMModel -> IMModel)
setWebSocket ws = FAE.diff { webSocket: Just $ WS ws }

setToken :: String -> Aff (IMModel -> IMModel)
setToken token = FAE.diff { token: Just token }

setName :: String -> Aff (IMModel -> IMModel)
setName name = pure $ \model@(IMModel { user }) -> SN.updateModel model $ _ {
        user = SN.updateUser user $ _ {
                name = name
        }
}
