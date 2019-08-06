module Test.Server.Main where

import Prelude

import Effect (Effect)
import Test.Server.Landing.Action as TSLA
import Test.Server.Token as TST
import Test.Unit (TestSuite)
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TSLA.tests
        TST.tests


