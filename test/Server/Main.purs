module Test.Server.Main where

import Prelude

import Test.Server.Landing.Action as TSLA
import Test.Server.Login.Action as TSLIA
import Test.Server.Token as TST
import Test.Server.IM.Action as TSIA
import Test.Unit (TestSuite)

tests :: TestSuite
tests = do
        TSLA.tests
        TST.tests
        TSLIA.tests
        TSIA.tests


