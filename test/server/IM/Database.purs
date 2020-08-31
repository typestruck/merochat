module Test.Server.IM.Database where

import Prelude


import Database.PostgreSQL (Query(..))
import Server.Database as SD
import Server.Landing.Database as SLD
import Run as R
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Server.IM.Database as SID
import Test.Unit.Assert as TUA
import Data.Tuple.Nested((/\))

tests :: TestSuite
tests = do
      TU.suite "im database" do
            TU.test "processMessage creates history" $
                  TS.serverAction $ do
                        userID <- SLD.createUser {
                              email: "e@a.com",
                              name: "sdsd",
                              password: "ss",
                              headline: "sd",
                              description: "ss"
                        }
                        anotherUserID <- SLD.createUser {
                              email: "e@aaa.com" ,
                              name: "sdsd",
                              password: "ss",
                              headline: "sd",
                              description: "ss"
                        }
                        id <- SID.insertMessage userID anotherUserID "oi"
                        R.liftAff $ TUA.equal userID id
                        count <- SD.scalar' (Query """select cast(count(1) as integer) as c from histories where sender = $1 and recipient = $2""") (userID /\ anotherUserID)
                        R.liftAff $ TUA.equal 1 count
