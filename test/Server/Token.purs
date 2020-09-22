module Test.Server.Token where

import Prelude

import Run as R
import Run.Reader as RR
import Server.Token as ST

import Shared.Unsafe as SU
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "token" do
            TU.test "encoding decoding" do
                  TS.serverAction do
                        let id = 23
                        { configuration :configuration } <- RR.ask
                        token <- ST.createToken id
                        userID <- SU.fromJust <$> R.liftEffect (ST.userIDFromToken configuration.tokenSecret token)
                        R.liftAff $ TUA.equal id userID
