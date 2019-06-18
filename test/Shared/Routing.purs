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
		U.test "fromRoute" $ do
			A.equal (R.fromRoute Landing) "/"
			A.equal (R.fromRoute Register) "/register"
			A.equal (R.fromRoute $ Login {next :Nothing}) "/login"
			--routing duplex automaticaly encodes url
			A.equal (R.fromRoute $ Login {next : Just "/im"}) "/login?next=%2Fim"
        	U.test "toRoute" $ do
			A.equal (R.toRoute "/") $ Right Landing
			A.equal (R.toRoute "/register") $ Right Register
			A.equal (R.toRoute "/login") <<< Right $ Login { next: Nothing }
			A.equal (R.toRoute "/login?next=/im") <<< Right $ Login { next: Just "/im" }
			A.assert "should not parse" <<< E.isLeft $ R.toRoute "mlkhhihsoad2342423kkkkk"
