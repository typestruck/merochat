module Test.Server.Token where

import Prelude

import Data.Maybe (Maybe(..))
import Run as R
import Run.Reader as RR
import Server.Environment (tokenSecret)
import Server.Landing.Database as SLD
import Server.Token as ST
import Test.Server as TS
import Test.Server.User (baseUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "token" do
            TU.test "encoding decoding" do
                  TS.serverAction do
                        id ← SLD.createUser $ baseUser { email = Just "b@b.com" }
                        token ← ST.createToken id
                        userId ← ST.userIdFromToken tokenSecret token
                        R.liftAff $ TUA.equal (Just id) userId
