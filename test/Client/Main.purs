module Test.Client.Main where

import Prelude

import Test.Unit (TestSuite)
import Test.Client.Im.Suggestion as TCIS
import Test.Client.Im.Chat as TCIC
import Test.Client.Im.Contacts as TCICN
import Test.Client.Im.Main as TCIM
import Test.Client.Im.History as TCIH

tests ∷ TestSuite
tests = do
      TCIS.tests
      TCIC.tests
      TCICN.tests
      TCIM.tests
      TCIH.tests
