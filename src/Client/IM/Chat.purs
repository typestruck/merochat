module Client.IM.Chat where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Flame (World)
import Web.Socket.WebSocket (WebSocket)

update :: World IMModel IMMessage -> IMModel -> SuggestionMessage -> Aff IMModel
update _ model =
        case _ of
                SetWebSocket webSocket -> setWebSocket model webSocket

setWebSocket :: IMModel -> WebSocket -> Aff IMModel
setWebSocket (IMModel model) ws = pure <<< IMModel $ model { webSocket = ws }
