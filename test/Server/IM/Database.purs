module Test.Server.IM.Database where

import Prelude
import Shared.Types
import Shared.IM.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row2(..))
import Effect (Effect)
import Server.Database as SD
import Server.Database.User as SDU
import Server.Landing.Action (invalidUserEmailMessage, emailAlreadyRegisteredMessage)
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Run as R
import Server.Token as ST
import Server.Types
import Test.Server as TS
import Data.Either(Either(..))
import Test.Unit (TestSuite)
import Data.Tuple(Tuple(..))
import Test.Unit as TU
import Server.IM.Database as SID
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TU.suite "im database" do
                TU.test "insertMessage creates history" $
                        TS.serverAction $ \_ -> do
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
                                (response :: Tuple PrimaryKey (Either IMUser PrimaryKey) ) <- SID.insertMessage userID anotherUserID "oi"
                                case response of
                                        Tuple _ (Right _) ->
                                                R.liftAff $ TU.failure "expected history creation"
                                        Tuple _ (Left (IMUser {id})) -> do
                                                R.liftAff $ TUA.equal userID id
                                                --for some reason Database.PostgreSql gets the count as a string
                                                count <- SD.scalar' (Query """select cast(count(1) as integer) as c from histories where sender = $1 and recipient = $2""") $ Row2  userID anotherUserID
                                                R.liftAff $ TUA.equal 1 count
                TU.test "insertMessage returns user if history exists" $
                        TS.serverAction $ \_ -> do
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
                                (_ :: Tuple PrimaryKey (Either IMUser PrimaryKey)) <- SID.insertMessage userID anotherUserID "oi"
                                (response :: Tuple PrimaryKey (Either IMUser PrimaryKey)) <- SID.insertMessage anotherUserID userID "ola"
                                case response of
                                        Tuple _ (Right sender) -> do
                                                R.liftAff $ TUA.equal anotherUserID sender
                                                count <- SD.scalar' (Query """select cast(count(1) as integer) as c from histories where sender = $1 and recipient = $2""") $ Row2 anotherUserID userID
                                                --zero as anotherUserID was already inserted as recipient
                                                R.liftAff $ TUA.equal 0 count
                                        Tuple _ (Left _) -> do
                                                R.liftAff $ TU.failure "expected history creation"

