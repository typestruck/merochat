module Test.Server.Login.Action where

import Droplet.Language
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Server.AccountValidation (invalidEmailMessage, invalidPasswordMessage)
import Server.Database as SD
import Server.Database.Users (_email, _visibility, users)
import Server.Landing.Database as SLD
import Server.Login.Action (invalidLogin)
import Server.Login.Action as SLIA
import Server.Token as ST
import Shared.User (ProfileVisibility(..))
import Test.Server as TS
import Test.Server.User (email, password)
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = do
      TU.suite "login actions" do
            let expectExpection rl = void $ SLIA.login rl

            TU.test "does not accept empty login and password"
                  $ TS.serverActionCatch (TS.catch invalidEmailMessage)
                  $ expectExpection
                        { email: ""
                        , password: ""
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept empty password"
                  $ TS.serverActionCatch (TS.catch invalidPasswordMessage)
                  $ expectExpection
                        { email
                        , password: ""
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept inexistent login"
                  $ TS.serverActionCatch (TS.catch invalidLogin)
                  $ expectExpection
                        { email
                        , password
                        , captchaResponse: Nothing
                        }

            TU.test "does not accept wrong password" $
                  TS.serverActionCatch (TS.catch invalidLogin) do
                        void $ SLD.createUser
                              { email
                              , password: "sf"
                              , name: "sdsd"
                              , headline: "sd"
                              , description: "ss"
                              }
                        SLIA.login
                              { email
                              , password: "sssss"
                              , captchaResponse: Nothing
                              }

            TU.test "creates session token" $
                  TS.serverAction do
                        let email2 = "email@email.com.jp"
                        hashed ← ST.hashPassword password
                        void $ SLD.createUser
                              { email: email2
                              , password: hashed
                              , name: "sdsd"
                              , headline: "sd"
                              , description: "ss"
                              }
                        SLIA.login
                              { email: email2
                              , password
                              , captchaResponse: Nothing
                              }

            TU.test "does not accept banned users" $
                  TS.serverActionCatch (TS.catch invalidLogin) do
                        void $ SLD.createUser
                              { email
                              , password: "sfccccccccc"
                              , name: "sdsd"
                              , headline: "sd"
                              , description: "ss"
                              }
                        SD.execute $ update users # set (_visibility .=. TemporarilyBanned) # wher (_email .=. email)
                        SLIA.login
                              { email
                              , password: "sfccccccccc"
                              , captchaResponse: Nothing
                              }
