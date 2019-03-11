module Test.Shared.Routing where

import Prelude
import Effect (Effect)
import Test.Unit as U
import Test.Unit.Main as M
import Test.Unit.Assert as A
import Data.Either(Either(..))
import Data.Either as E
import Shared.Routing as R
import Shared.Types
import Data.Maybe(Maybe(..))

tests :: Effect Unit
tests = M.runTest $ do
	U.suite "routing" $ do
		U.test "fromResouce" $ do
			A.equal (R.fromResource "/") $ Right Landing
			A.equal (R.fromResource "/register") $ Right Register
			A.equal (R.fromResource "/login") <<< Right $ Login { next: Nothing }
			A.equal (R.fromResource "/login?next=/im") <<< Right $ Login { next: Just "/im" }
			A.assert "should not parse" <<< E.isLeft $ (R.fromResource "mlkhhihsoad2342423kkkkk")
        	U.test "toResource" $ do
			A.equal (R.toResource Landing) "/"
			A.equal (R.toResource Register) "/register"
			A.equal (R.toResource $ Login {next :Nothing}) "/login"
			--routing duplex automaticaly encodes url
			A.equal (R.toResource $ Login {next : Just "/im"}) "/login?next=%2Fim"
