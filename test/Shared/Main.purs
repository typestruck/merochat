module Test.Shared.Main where

import Prelude
import Effect (Effect)
import Test.Shared.Routing as TSR

main :: Effect Unit
main = TSR.tests
