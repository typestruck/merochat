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
			A.equal (R.fromRoute "/") $ Right Landing
			A.equal (R.fromRoute "/register") $ Right Register
			A.equal (R.fromRoute "/login") <<< Right $ Login { next: Nothing }
			A.equal (R.fromRoute "/login?next=/im") <<< Right $ Login { next: Just "/im" }
			A.assert "should not parse" <<< E.isLeft $ (R.fromRoute "mlkhhihsoad2342423kkkkk")
        	U.test "toRoute" $ do
			A.equal (R.toRoute Landing) "/"
			A.equal (R.toRoute Register) "/register"
			A.equal (R.toRoute $ Login {next :Nothing}) "/login"
			--routing duplex automaticaly encodes url
			A.equal (R.toRoute $ Login {next : Just "/im"}) "/login?next=%2Fim"
