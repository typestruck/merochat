module Test.Client.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite)
import Test.Client.IM.Suggestion as TCIS

tests :: TestSuite
tests = TCIS.tests