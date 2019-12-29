module Client.IM.Main where

import Prelude
import Shared.Types

import Client.Common (tokenKey)
import Client.Common as CC
import Client.IM.Chat as CIC
import Client.IM.Suggestion as CIS
import Data.Argonaut.Core as DAC
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as EC
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame (QuerySelector(..), World)
import Flame as F
import Shared.IM.View as SIV
import Shared.WebSocket.Options (port)
import Signal.Channel as SC
import Web.Event.EventTarget as WET
import Web.Socket.Event.EventTypes (onOpen)
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

        token <- CC.getItem tokenKey
        webSocket <- WSW.create ("ws://localhost:" <> show port) []
        SC.send channel <<< Just <<< MM $ SetWebSocket webSocket
        SC.send channel <<< Just <<< MM $ SetToken token

        listener <- WET.eventListener $ const (WSW.sendString webSocket <<< DAC.stringify <<< DAEGR.genericEncodeJson $ Connect token)
        WET.addEventListener onOpen listener false $ WSW.toEventTarget webSocket

        editor <- loadEditor
        EU.runEffectFn2 keyHandled_ editor $ EU.mkEffectFn1 (SC.send channel <<< Just <<< CM <<< SendMessage)

update :: World IMModel IMMessage -> IMModel -> IMMessage -> Aff IMModel
update world model =
        case _ of
                SM message -> CIS.update world model message
                CM message -> CIC.update world model message
                MM message -> set model message
        where set model =
                case _ of
                        SetWebSocket webSocket -> setWebSocket model webSocket
                        SetToken token -> setToken model token

setWebSocket :: IMModel -> WebSocket -> Aff IMModel
setWebSocket (IMModel model) ws = pure <<< IMModel $ model { webSocket = Just $ WS ws }

setToken :: IMModel -> String -> Aff IMModel
setToken (IMModel model) token = pure <<< IMModel $ model { token = Just token }
