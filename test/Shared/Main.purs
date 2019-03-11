module Test.Shared.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Shared.Routing as R

main :: Effect Unit
main = do
        R.tests
