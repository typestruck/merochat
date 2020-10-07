module Test.Server.Login.Action where

import Prelude
import Server.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row0(..))
import Server.AccountValidation (invalidEmailMessage, invalidPasswordMessage)
import Server.Database as SD
import Server.Landing.Database as SLD
import Server.Login.Action (invalidLogin)
import Server.Login.Action as SLIA
import Server.Token as ST
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

email :: String
email = "e@a.com"

password :: String
password = "hunter12"

tests :: TestSuite
tests = do
      TU.suite "login actions" do
            let  expectExpection rl = void $ SLIA.login rl

            TU.test "login does not accept empty login and password" $
                  TS.serverActionCatch (TS.catch invalidEmailMessage) $ expectExpection {
                        email: "",
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "login does not accept empty password" $
                  TS.serverActionCatch (TS.catch invalidPasswordMessage) $ expectExpection {
                        email,
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "login does not accept inexsting login" $
                  TS.serverActionCatch (TS.catch invalidLogin) $ expectExpection {
                        email,
                        password,
                        captchaResponse: Nothing
                  }

            TU.test "login does not accept wrong password" $
                  TS.serverActionCatch (TS.catch invalidLogin) do
                        void $ SLD.createUser {
                              email,
                              password: "sf",
                              name: "sdsd",
                              headline: "sd",
                              description: "ss"
                        }
                        SLIA.login {
                              email,
                              password: "sssss",
                              captchaResponse: Nothing
                        }

            TU.test "creates session token" $
                  TS.serverAction do
                        let email2 = "email@email.com.jp"
                        hashed <- ST.hashPassword password
                        void $ SLD.createUser {
                              email: email2,
                              password: hashed,
                              name: "sdsd",
                              headline: "sd",
                              description: "ss"
                        }
                        SLIA.login {
                              email: email2,
                              password ,
                              captchaResponse: Nothing
                        }