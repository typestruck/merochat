module Test.Client.Main where

import Prelude

import Test.Unit (TestSuite)
import Test.Client.IM.Suggestion as TCIS
import Test.Client.IM.Chat as TCIC
import Test.Client.IM.Contacts as TCICN


tests :: TestSuite
tests = do
        TCIS.tests
        TCIC.tests
        TCICN.tests