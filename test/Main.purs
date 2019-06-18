module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Client.Main as CM
import Test.Server.Main as SM
import Test.Shared.Main as SSM

main :: Effect Unit
main = do
        CM.main
        SM.main
        SSM.main