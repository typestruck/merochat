module Test.Server.Landing.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row0(..), Row1(..))
import Run as R
import Server.Database as SD
import Server.Database.User as SDU
import Server.Landing.Action (invalidUserEmailMessage, emailAlreadyRegisteredMessage)
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Server.Token as ST
import Shared.Unsafe as SU
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

email :: String
email = "e@a.com"

tests :: TestSuite
tests = do
        TU.suite "landing actions" do
                let registerExceptionTest rl = do
                        void $ SLA.register rl
                        users <- userCount
                        R.liftAff $ TUA.equal 0 users

                TU.test "register does not accept empty fields" $
                        TS.serverActionCatch (TS.catch invalidUserEmailMessage) $ registerExceptionTest {
                                email: "",
                                password: "",
                                captchaResponse: Nothing
                        }

                TU.test "register does not accept empty password" $
                        TS.serverActionCatch (TS.catch invalidUserEmailMessage) $ registerExceptionTest {
                                email,
                                password: "",
                                captchaResponse: Nothing
                        }


                TU.test "register does not accept existing email" $
                        TS.serverActionCatch (TS.catch emailAlreadyRegisteredMessage) $ do
                                void $ SLD.createUser {
                                        email,
                                        name: "sdsd",
                                        password: "ss",
                                        headline: "sd",
                                        description: "ss"
                                }
                                registerExceptionTest $ {
                                        email,
                                        password: "ss",
                                        captchaResponse: Nothing
                                }

                TU.test "register creates user" $
                        TS.serverAction $ do
                                let password = "hunter12"
                                _ <- SLA.register $ {
                                        email,
                                        password,
                                        captchaResponse: Nothing
                                }
                                maybeUser <- SDU.userBy (Email email)
                                case maybeUser of
                                        Nothing -> R.liftAff $ TU.failure "user not created!"
                                        Just (RegisterLoginUser user) -> do
                                                hashed <- ST.hashPassword password
                                                R.liftAff $ TUA.equal hashed user.password

                TU.test "register creates karma" $
                        TS.serverAction $ do
                                let password = "hunter12"
                                void $ SLA.register {
                                        email,
                                        password,
                                        captchaResponse: Nothing
                                }
                                RegisterLoginUser { id } <- SU.fromJust <$> (SDU.userBy $ Email email)
                                --REFACTOR: starting karma amount should be in options
                                count <- SD.scalar' (Query "select count(1) from karma_histories where target = $1 and amount = 5") $ Row1 id
                                R.liftAff $ TUA.equal 1 count



