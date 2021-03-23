module Test.Shared.DateTime where

import Prelude

import Data.Maybe (Maybe(..))
import Shared.DateTime as SD
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "calculating age" do
            TU.test "" do
                  TUA.equal 3 4
      TU.suite "displaying message date time" do
            TU.test "ago shows yesterday" do
                  TUA.equal 3 4
            TU.test "ago shows day of the week" do
                  TUA.equal 3 4
            TU.test "ago shows full date" do
                  TUA.equal 3 4
            TU.test "agoWithTime includes time" do
                  TUA.equal 3 4