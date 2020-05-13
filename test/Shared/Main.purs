module Test.Shared.Main where

import Prelude

import Effect (Effect)
import Test.Shared.Router as TSR
import Test.Unit (TestSuite)

tests :: TestSuite
tests = TSR.tests
