module Client.IM.Chat where

import Prelude
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Array ((!!))
import Data.Array as DA
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame (World)
import Partial.Unsafe as PU
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSW

update :: World IMModel IMMessage -> IMModel -> ChatMessage -> Aff IMModel
update _ model =
        case _ of
                SendMessage content -> sendMessage model content

--needs to clean the editor
sendMessage :: IMModel -> String -> Aff IMModel
sendMessage (IMModel model@{webSocket: Just (WS webSocket), token: Just token, temporaryID, chatting: Just chatting, suggestions}) content = do
        let (IMUser user) = PU.unsafePartial $ DM.fromJust (suggestions !! chatting)
            newTemporaryID = temporaryID + 1
            updatedChatting = IMUser $ user { history = DA.snoc user.history $ History { content } }
        liftEffect <<< WSW.sendString webSocket <<< DAC.stringify <<< DAEGR.genericEncodeJson $ Message {
                id: PrimaryKey $ DI.fromInt newTemporaryID,
                user: user.id,
                token: token,
                content
        }
        pure <<< IMModel $ model {
                temporaryID = newTemporaryID,
                suggestions = PU.unsafePartial (DM.fromJust $ DA.updateAt chatting updatedChatting suggestions)
        }
sendMessage _ _ = EA.throwError $ EA.error "Invalid sendMessage state"