module Test.Client.Im.Suggestion where

import Prelude

import Client.Im.Main as CIM
import Client.Im.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Data.Tuple as DT
import Effect.Class as EC
import Effect.Console as ECL
import Test.Client.Model (anotherImUser, contact, imUser, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Unsafe.Coerce as SU

tests ∷ TestSuite
tests = do
      pure unit