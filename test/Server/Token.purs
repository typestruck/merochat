module Test.Server.Token where

import Prelude

import Run as R
import Run.Reader as RR
import Server.Token as ST

import Shared.Unsafe as SU
import Test.Server as TS
import Test.Unit (TestSuite)
import Server.Landing.Database as SLD
import Test.Unit as TU
import Data.Maybe (Maybe(..))
import Test.Server.User (baseUser)
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "token" do
            TU.test "encoding decoding" do
                  TS.serverAction do
                        { configuration: { tokenSecret } } ← RR.ask
                        id ← SLD.createUser $ baseUser { email = Just "b@b.com" }
                        token ← ST.createToken id
                        userId ← ST.userIdFromToken tokenSecret token
                        R.liftAff $ TUA.equal (Just id) userId
