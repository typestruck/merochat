module Client.Im.Asks where

import Prelude

import Client.AppId (imAppId)
import Client.File as CCF
import Client.Network (request)
import Client.Network as CCN
import Client.Im.Flame (MoreMessages, NoMessages, NextMessage)
import Client.Im.WebSocket as CIW
import Control.Alt ((<|>))
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
