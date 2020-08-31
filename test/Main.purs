module Test.Main where

import Prelude

import Effect (Effect)
import Test.Client.Main as TCM
import Test.Server.Main as TSM
import Test.Unit.Main as TUM

main :: Effect Unit
main = TUM.runTest do
      TSM.tests
      TCM.tests

