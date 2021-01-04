module Test.Server.Login.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Server.AccountValidation (invalidEmailMessage, invalidPasswordMessage)
import Server.Landing.Database as SLD
import Server.Login.Action (invalidLogin)
import Server.Login.Action as SLIA
import Server.Token as ST
import Test.Server as TS
import Test.Server.User (email, password)
import Test.Unit (TestSuite)
import Test.Unit as TU

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

            TU.test "login does not accept inexesting login" $
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