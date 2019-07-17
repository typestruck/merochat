module Test.Server.Main where

import Prelude
import Effect (Effect)
import Test.Server.Landing.Action as TSLA
import Test.Server.Bender as TSB

main :: Effect Unit
main = do
        TSLA.tests
        TSB.tests
