module Test.Server.Main where

import Prelude
import Effect (Effect)
import Test.Server.Landing.Action as TSLA

main :: Effect Unit
main = do
        TSLA.tests
