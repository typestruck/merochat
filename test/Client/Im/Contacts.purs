module Test.Client.Im.Contacts where

import Prelude
import Shared.DateTime
import Shared.Im.Types
import Shared.User

import Client.Im.Contacts as CICN
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe ((!@))
import Test.Client.Model (contact, historyMessage, imUser, imUserId, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      pure unit