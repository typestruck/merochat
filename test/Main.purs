module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process as NP
import Test.Client.Main as TCM
import Test.Server.Main as TSM
import Test.Unit.Main as TUM

main :: Effect Unit
main = do
      cli <- NP.lookupEnv "CLI"
      TUM.runTest do
            unless (cli == Just "true") TSM.tests
            TCM.tests

