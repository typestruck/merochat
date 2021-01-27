module Test.Client.Main where

import Prelude

import Test.Unit (TestSuite)
import Test.Client.IM.Suggestion as TCIS
import Test.Client.IM.Chat as TCIC
import Test.Client.IM.Contacts as TCICN
import Test.Client.IM.Main as TCIM
import Test.Client.IM.History as TCIH

tests :: TestSuite
tests = do
      TCIS.tests
      TCIC.tests
      TCICN.tests
      TCIM.tests
      TCIH.tests
