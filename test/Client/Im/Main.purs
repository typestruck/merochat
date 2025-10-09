module Test.Client.Im.Main where

import Prelude

import Client.Im.WebSocket.Events as CIWE
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.Availability (Availability(..))
import Shared.Content (Content(..))
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Im.Types (MessageStatus(..), WebSocketPayloadClient(..), WebSocketPayloadServer(..))
import Shared.ResponseError (DatabaseError(..))
import Shared.Unsafe ((!@))
import Test.Client.Model (anotherImUserId, contact, contactId, historyMessage, imUser, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      pure unit