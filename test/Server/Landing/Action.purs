module Test.Server.Landing.Action where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row0(..))
import Effect (Effect)
import Run as R
import Server.Database as SD
import Server.Database.User as SDU
import Server.Landing.Action (invalidUserEmailMessage, emailAlreadyRegisteredMessage)
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Server.Token as ST
import Server.Types
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

email :: String
email = "e@a.com"

tests :: TestSuite
tests = do
        TU.suite "landing actions" $ do
                TU.test "register - validation" $ do
                        let     catch expected (BadRequest {reason}) = R.liftAff $ TUA.equal expected reason
                                catch _ other = R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other

                                registerExceptionTest rl = do
                                        _ <- SLA.register "" rl
                                        users <- userCount
                                        R.liftAff $ TUA.equal 0 users

                        TS.serverActionCatch (catch invalidUserEmailMessage)
                                $ \_ -> registerExceptionTest $ RegisterLogin {
                                        email: "",
                                        password: "",
                                        captchaResponse: Nothing
                                }

                        TS.serverActionCatch (catch invalidUserEmailMessage)
                                $ \_ -> registerExceptionTest $ RegisterLogin {
                                        email,
                                        password: "",
                                        captchaResponse: Nothing
                                }

                        TS.serverActionCatch (catch emailAlreadyRegisteredMessage)
                                $ \_ -> do
                                        _ <- SLD.createUser {
                                                email,
                                                name: "sdsd",
                                                password: "ss",
                                                headline: "sd",
                                                description: "ss"
                                        }
                                        registerExceptionTest $ RegisterLogin {
                                                email,
                                                password: "ss",
                                                captchaResponse: Nothing
                                        }
                TU.test "register - user creation" $
                        TS.serverAction $ \_ -> do
                                let password = "hunter12"
                                _ <- SLA.register "" $ RegisterLogin {
                                                email,
                                                password,
                                                captchaResponse: Nothing
                                        }
                                maybeUser <- SDU.userBy (Email email)
                                case maybeUser of
                                        Nothing -> R.liftAff $ TU.failure "user not created!"
                                        Just (User user) -> do
                                                hashed <- ST.hashPassword password
                                                R.liftAff $ TUA.equal hashed user.password
