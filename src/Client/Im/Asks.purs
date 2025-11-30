module Client.Im.Asks where

import Prelude

import Client.AppId (imAppId)
import Client.File as CCF
import Client.Im.Flame (MoreMessages, NoMessages, NextMessage)
import Client.Im.WebSocket as CIW
import Client.Network (request)
import Client.Network as CCN
import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as TDS
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class as EC
import Shared.Content (Content(..))
import Shared.Im.Types (For(..), ImMessage(..), ImModel, PostMode(..), RetryableRequest(..), SelectedImage, WebSocketPayloadServer(..))
import Shared.Modal (Modal(..), SpecialModal(..))
import Shared.Post (Post)
import Shared.Resource (maxImageSize)
import Shared.Unsafe as SU
import Shared.User (ProfileTab(..))
import Type.Proxy (Proxy(..))
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as WDE
import Web.HTML.HTMLInputElement as WHI
import Web.Socket.WebSocket (WebSocket)

fetchAsks ∷ Int → ImModel → MoreMessages
fetchAsks userId model = model /\ []

setAsk ∷ Maybe String → ImModel → MoreMessages
setAsk value model = model { asks = model.asks { question = value } } /\ []

sendAsk ∷ Int → ImModel → MoreMessages
sendAsk userId model = model { asks = model.asks { freeToSend = false } } /\ [ send ]
      where
      send = do
            response ← CCN.silentResponse $ request.asks.post { body: { userId, question: SU.fromJust model.asks.question } }
            pure <<< Just $ AfterSendAsk userId response.allowed

afterSendAsk ∷ Int → Boolean → ImModel → MoreMessages
afterSendAsk userId allowed model =
      model
            { asks = model.asks
                    { freeToSend = true
                    , question = Nothing
                    , sent = updatedSent
                    , unallowed = updatedUnallowed
                    }
            } /\ []
      where
      updatedSent /\ updatedUnallowed =
            if allowed then
                  (userId : model.asks.sent) /\ model.asks.unallowed
            else
                  model.asks.sent /\ (userId : model.asks.unallowed)
