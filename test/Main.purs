module Test.Main where

import Prelude

import Effect (Effect)
import Test.Client.Main as TCM
import Test.Server.Main as TSM
import Test.Shared.Main as TSSM
import Test.Unit.Main as TUM

main :: Effect Unit
main = TUM.runTest $ do
        TSSM.tests
        TSM.tests
        TCM.tests

