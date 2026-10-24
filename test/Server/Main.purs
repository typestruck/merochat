module Test.Server.Main where

import Prelude

import Test.Server.Landing.Action as TSLA
import Test.Server.Landing.Handler as TSLH
import Test.Server.Profile.Action as TSPA
import Test.Server.Unsubscribe as TSU
import Test.Server.Generate as TSG
import Test.Server.Login.Action as TSLIA
import Test.Server.Token as TST
import Test.Server.Im.Action as TSIA
import Test.Unit (TestSuite)

tests ∷ TestSuite
tests = do
      TSLA.tests
      TSLH.tests
      TST.tests
      TSG.tests
      TSU.tests
      TSLIA.tests
      TSIA.tests
      TSPA.tests
