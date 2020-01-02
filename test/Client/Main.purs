module Test.Client.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite)
import Test.Client.IM.Suggestion as TCIS
import Test.Client.IM.Chat as TCIC

tests :: TestSuite
tests = do
        TCIS.tests
        TCIC.tests