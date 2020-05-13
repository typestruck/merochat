module Test.Shared.Router where

import Prelude
import Shared.Types

import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.Router as SR
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TU.suite "routing" $ do
                TU.test "fromRouteAbsolute" $ do
                        TUA.equal (SR.fromRouteAbsolute Landing) "/"
                        TUA.equal (SR.fromRouteAbsolute Register) "/register"
                        TUA.equal (SR.fromRouteAbsolute $ Login {next :Nothing}) "/login"
                        --routing duplex automaticaly encodes url
                        TUA.equal (SR.fromRouteAbsolute $ Login {next : Just "/im"}) "/login?next=%2Fim"
                TU.test "toRoute" $ do
                        TUA.equal (SR.toRoute "/") $ Right Landing
                        TUA.equal (SR.toRoute "/register") $ Right Register
                        TUA.equal (SR.toRoute "/login") <<< Right $ Login { next: Nothing }
                        TUA.equal (SR.toRoute "/login?next=/im") <<< Right $ Login { next: Just "/im" }
                        TUA.assert "should not parse" <<< DE.isLeft $ SR.toRoute "mlkhhihsoad2342423kkkkk"
