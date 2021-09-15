module Test.Shared.Main where

import Prelude

import Test.Unit (TestSuite)
import Test.Shared.DateTime as TSD

tests ∷ TestSuite
tests =
      TSD.tests