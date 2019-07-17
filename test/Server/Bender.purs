module Test.Server.Bender where

import Prelude
import Effect (Effect)
import Test.Unit as TU
import Test.Unit.Main as TUM
import Test.Unit.Assert as TUA
import Data.Either(Either(..))
import Data.Either as DE
import Shared.Types
import Data.Maybe(Maybe(..))

tests = TUM.runTest $ do
        TU.suite "routing" $ do
                TU.test "fromRouteAbsolute" $ do
                        TUA.equal  2 3