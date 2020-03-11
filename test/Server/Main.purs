module Test.Server.Main where

import Prelude

import Test.Server.Landing.Action as TSLA
import Test.Server.Login.Action as TSLIA
import Test.Server.IM.Database as TSID
import Test.Server.Token as TST
import Test.Unit (TestSuite)

tests :: TestSuite
tests = do
        TSLA.tests
        TST.tests
        TSLIA.tests
        TSID.tests


