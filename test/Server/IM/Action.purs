module Test.Server.IM.Action where

import Prelude
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (Query(..), Row2(..))
import Effect (Effect)
import Run as R
import Server.Database as SD
import Server.Database.User as SDU
import Server.IM.Database as SID
import Server.Landing.Action (invalidUserEmailMessage, emailAlreadyRegisteredMessage)
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Server.Token as ST
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TU.suite "im actions" $ do
                TU.test "contactList" $
                        TUA.equal 1 2