module Test.Server.IM.Action where

import Prelude
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im actions" do
            TU.test "contactList paginates contacts" $
                  TUA.equal 1 2
            TU.test "contactList orders contacts" $
                  TUA.equal 1 2