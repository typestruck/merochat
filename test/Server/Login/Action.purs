module Test.Server.Login.Action where

import Droplet.Language
import Prelude

import Data.Maybe (Maybe(..))
import Server.AccountValidation (invalidEmailMessage, invalidPasswordMessage)
import Server.Database as SD
import Server.Database.Users (_email, _visibility, users)
import Server.Landing.Database as SLD
import Server.Login.Action (invalidLogin)
import Server.Login.Action as SLIA
import Server.Token as ST
import Shared.User (ProfileVisibility(..))
import Test.Server as TS
import Test.Server.User (baseUser, email, password)
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = do
      TU.suite "login actions" do
            let expectException rl = void $ SLIA.login rl

            TU.test "does not accept empty login and password"
                  $ TS.serverActionCatch (TS.catch invalidEmailMessage)
                  $ expectException
                        { email: ""
                        , password: ""
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept empty password"
                  $ TS.serverActionCatch (TS.catch invalidPasswordMessage)
                  $ expectException
                        { email
                        , password: ""
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept inexistent login"
                  $ TS.serverActionCatch (TS.catch invalidLogin)
                  $ expectException
                        { email
                        , password
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept wrong password" $
                  TS.serverActionCatch (TS.catch invalidLogin) do
                        void $ SLD.createUser baseUser
                        SLIA.login
                              { email
                              , password: "sssss"
                              , captchaResponse: Nothing
                              }

            TU.test "creates session token" $
                  TS.serverAction do
                        let email2 = "email@email.com.jp"
                        hashed ← ST.hashPassword password
                        void $ SLD.createUser baseUser
                              { email = Just email2
                              , password = Just hashed
                              }
                        SLIA.login
                              { email: email2
                              , password
                              , captchaResponse: Nothing
                              }

            TU.test "does not accept banned users" $
                  TS.serverActionCatch (TS.catch invalidLogin) do
                        void $ SLD.createUser
                              baseUser
                        SD.execute $ update users # set (_visibility .=. TemporarilyBanned) # wher (_email .=. email)
                        SLIA.login
                              { email
                              , password
                              , captchaResponse: Nothing
                              }
