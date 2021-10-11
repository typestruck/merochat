module Test.Main where

import Prelude

import Effect (Effect)
import Server.Configuration as SC
import Test.Client.Main as TCM
import Test.Server.Main as TSM
import Test.Shared.Main as TSRM
import Test.Unit.Main as TUM

main ∷ Effect Unit
main = do
      TUM.runTest do
            TSM.tests
            TSRM.tests
            TCM.tests
