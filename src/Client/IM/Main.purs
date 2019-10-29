module Client.IM.Main where

import Prelude
import Shared.Types

import Client.IM.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Shared.WebSocket.Options(port)
import Flame (QuerySelector(..), World)
import Flame as F
import Shared.IM.View as SIV
import Signal.Channel as SC
import Web.Socket.WebSocket as WSW

foreign import data Editor :: Type
foreign import loadEditor :: Effect Editor

main :: Effect Unit
main = do
        channel <- F.resumeMount (QuerySelector ".im") {
                view: SIV.view,
                init: Nothing,
                update
        }

        webSocket <- WSW.create ("ws://localhost:" <> show port) []
        SC.send channel <<< Just <<< CM <<< SetWebSocket webSocket

        editor <- loadEditor

        pure unit

update :: World IMModel IMMessage -> IMModel -> IMMessage -> Aff IMModel
update world model =
                case _ of
                        SM message -> CIS.update world model message
                        CM message -> CIC.update world model message