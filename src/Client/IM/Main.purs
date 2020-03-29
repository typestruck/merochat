module Client.IM.Main where

import Prelude
import Shared.Types

import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Scroll as CISR
import Client.IM.Suggestion as CIS
import Control.Monad.Except as CME
import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as EC
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame (QuerySelector(..), World)
import Flame as F
import Foreign as FO
import Partial.Unsafe as UP
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Unsafe as SU
import Shared.WebSocketOptions (port)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.Socket.Event.EventTypes (onOpen, onMessage)
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

foreign import data Editor :: Type
foreign import loadEditor :: Effect Editor
foreign import keyHandled_ :: EffectFn2 Editor (EffectFn1 String Unit) Unit

main :: Effect Unit
main = void do
        channel <- F.resumeMount (QuerySelector ".im") {
                view: SIV.view,
                init: Nothing,
                update
        }

        token <- CCS.getItem tokenKey
        webSocket <- WSW.create ("ws://localhost:" <> show port) []
        SC.send channel <<< Just <<< MM $ SetWebSocket webSocket
        SC.send channel <<< Just <<< MM $ SetToken token

        openListener <- WET.eventListener $ const (WSW.sendString webSocket <<< SJ.toJSON $ Connect token)
        WET.addEventListener onOpen openListener false $ WSW.toEventTarget webSocket

        messageListener <- WET.eventListener $ \event -> do
                let possiblePayload = CME.runExcept <<< FO.readString <<< WSEM.data_ <<< SU.unsafeFromJust "client.im.main" $ WSEM.fromEvent event
                case possiblePayload of
                        Left e -> EC.log ("bogus payload " <> show (map FO.renderForeignError e))
                        Right payload -> do
                                case SJ.fromJSON payload of
                                        Left e -> EC.log $ "bogus payload " <> show e
                                        Right r -> SC.send channel <<< Just <<< CM $ ReceiveMessage r

        WET.addEventListener onMessage messageListener false $ WSW.toEventTarget webSocket

        editor <- loadEditor
        EU.runEffectFn2 keyHandled_ editor $ EU.mkEffectFn1 (SC.send channel <<< Just <<< CM <<< SendMessage)

        CISR.scrollLastMessage

update :: World IMModel IMMessage -> IMModel -> IMMessage -> Aff IMModel
update world model =
        case _ of
                SM message -> CIS.update world model message
                CM message -> CIC.update world model message
                CNM message -> CICN.update world model message
                MM message -> set model message
        where set model =
                case _ of
                        SetWebSocket webSocket -> setWebSocket model webSocket
                        SetToken token -> setToken model token

setWebSocket :: IMModel -> WebSocket -> Aff IMModel
setWebSocket (IMModel model) ws = pure <<< IMModel $ model { webSocket = Just $ WS ws }

setToken :: IMModel -> String -> Aff IMModel
setToken (IMModel model) token = pure <<< IMModel $ model { token = Just token }
