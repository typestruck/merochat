module Test.Server.Token where

import Prelude

import Effect (Effect)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

main :: Effect Unit
main = TUM.runTest $ do
	TU.suite "routing" $ do
		TU.test "fromRouteAbsolute" $ do
			TUA.equal 2 4