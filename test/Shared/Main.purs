module Test.Shared.Main where

import Prelude

import Effect (Effect)
import Test.Shared.Routing as TSR
import Test.Unit (TestSuite)

tests :: TestSuite
tests = TSR.tests
