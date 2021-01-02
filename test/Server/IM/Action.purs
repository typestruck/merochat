module Test.Server.IM.Action where

import Prelude

import Data.Array as DA
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..))
import Run as R
import Server.Database as SD
import Server.IM.Action as SIA
import Server.Landing.Database as SLD
import Shared.Types (MessageContent(..))
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suiteOnly "im actions" do
            TU.test "suggest filters out blocked users" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        suggestions <- SIA.suggest userID 0
                        R.liftAff <<< TUA.equal 1 $ DA.length suggestions

                        void $ SIA.blockUser userID anotherUserID
                        suggestions <- SIA.suggest userID 0
                        R.liftAff <<< TUA.equal 0 $ DA.length suggestions

            TU.testOnly "suggest filters out blocker users" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        suggestions <- SIA.suggest userID 0
                        R.liftAff <<< TUA.equal 1 $ DA.length suggestions

                        void $ SIA.blockUser userID anotherUserID
                        suggestions <- SIA.suggest anotherUserID 0
                        R.liftAff <<< TUA.equal 0 $ DA.length suggestions

            TU.test "listContacts matches contact and chat history" $
                  TUA.equal 1 2

            TU.test "listContacts orders contacts" $
                  TUA.equal 1 2

            TU.test "processMessage creates history" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple id _ <- SIA.processMessage userID anotherUserID 2 $ Text "oi"
                        R.liftAff $ TUA.equal userID id
                        count <- SD.scalar' (Query """select count(1) as c from histories where sender = $1 and recipient = $2""") (userID /\ anotherUserID)
                        R.liftAff $ TUA.equal 1 count

            TU.test "processMessage sanitizes input" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple id message <- SIA.processMessage userID anotherUserID 2 $ Text "oi"
                        R.liftAff $ TUA.equal userID id
                        count <- SD.scalar' (Query """select count(1) as c from histories where sender = $1 and recipient = $2""") (userID /\ anotherUserID)
                        R.liftAff $ TUA.equal 1 count

            TU.test "processMessage accepts files" $
                  TUA.equal 3 5

            TU.test "processMessage does not accept files too large" $
                  TUA.equal 3 5

            TU.test "listMissedEvents finds missed messages" $
                  TUA.equal 3 5

            TU.test "listMissedEvents finds missed contacts" $
                  TUA.equal 3 5

            TU.test "listMissedEvents finds temporary ids" $
                  TUA.equal 3 5
      where setUpUsers = do
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
                  pure $ Tuple userID anotherUserID