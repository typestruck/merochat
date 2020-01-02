module Test.Server.Login.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row0(..))
import Run as R
import Server.Database as SD
import Server.Database.User as SDU
import Server.Landing.Database as SLD
import Server.Login.Action (invalidLogin, invalidUserEmailMessage)
import Server.Login.Action as SLIA
import Server.Token as ST
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

email :: String
email = "e@a.com"

password :: String
password = "hunter12"

tests :: TestSuite
tests = do
         TU.suite "login actions" $ do
                TU.test "login - validation" $ do
                        let     catch expected (BadRequest {reason}) = R.liftAff $ TUA.equal expected reason
                                catch _ other = R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other

                                expectExpection rl = void $ SLIA.login rl

                        TS.serverActionCatch (catch invalidUserEmailMessage)
                                $ \_ -> expectExpection $ RegisterLogin {
                                        email: "",
                                        password: "",
                                        captchaResponse: Nothing
                                }

                        TS.serverActionCatch (catch invalidUserEmailMessage)
                                $ \_ -> expectExpection $ RegisterLogin {
                                        email,
                                        password: "",
                                        captchaResponse: Nothing
                                }

                        TS.serverActionCatch (catch invalidLogin)
                                $ \_ -> expectExpection $ RegisterLogin {
                                        email,
                                        password,
                                        captchaResponse: Nothing
                                }

                        TS.serverActionCatch (catch invalidLogin)
                                $ \_ -> void do
                                        _ <- SLD.createUser {
                                                email,
                                                password: "sf",
                                                name: "sdsd",
                                                headline: "sd",
                                                description: "ss"
                                        }
                                        SLIA.login $ RegisterLogin {
                                                email,
                                                password: "sssss",
                                                captchaResponse: Nothing
                                        }

                TU.test "login - token" $
                        TS.serverAction $ \_ -> void do
                                let email2 = "email@email.com.jp"
                                hashed <- ST.hashPassword password
                                PrimaryKey id <- SLD.createUser {
                                                email: email2,
                                                password: hashed,
                                                name: "sdsd",
                                                headline: "sd",
                                                description: "ss"
                                }

                                SLIA.login $ RegisterLogin {
                                                email: email2,
                                                password ,
                                                captchaResponse: Nothing
                                }