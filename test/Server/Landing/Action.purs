module Test.Server.Landing.Action where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row1(..))
import Run as R
import Server.AccountValidation (emailAlreadyRegisteredMessage, invalidEmailMessage, invalidPasswordMessage)
import Server.Database as SD
import Server.Database.User as SDU
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Server.Token as ST
import Shared.Unsafe as SU
import Test.Server as TS
import Test.Server.User (email, password)
import Test.Server.User as STU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "landing actions" do
            let registerExceptionTest rl = do
                  void $ SLA.register rl
                  users <- STU.userCount
                  R.liftAff $ TUA.equal 0 users

            TU.test "register does not accept empty fields" $
                  TS.serverActionCatch (TS.catch invalidEmailMessage) $ registerExceptionTest {
                        email: "",
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "register does not accept empty password" $
                  TS.serverActionCatch (TS.catch invalidPasswordMessage) $ registerExceptionTest {
                        email,
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "register does not accept existing email" $
                  TS.serverActionCatch (TS.catch emailAlreadyRegisteredMessage) $ do
                        void $ SLD.createUser {
                              email,
                              name: "sdsd",
                              password,
                              headline: "sd",
                              description: "ss"
                        }
                        registerExceptionTest $ {
                              email,
                              password,
                              captchaResponse: Nothing
                        }

            TU.test "register creates user" $
                  TS.serverAction $ do
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
                        void $ SLA.register {
                              email,
                              password,
                              captchaResponse: Nothing
                        }
                        RegisterLoginUser { id } <- SU.fromJust <$> (SDU.userBy $ Email email)
                        history <- SD.scalar' (Query "select count(1) from karma_histories where target = $1") $ Row1 id
                        R.liftAff $ TUA.equal 1 history
                        leaderboard <- SD.scalar' (Query "select count(1) from karma_leaderboard where ranker = $1") $ Row1 id
                        R.liftAff $ TUA.equal 1 leaderboard

            TU.test "register creates suggestion" $
                  TS.serverAction $ do
                        void $ SLA.register {
                              email,
                              password,
                              captchaResponse: Nothing
                        }
                        RegisterLoginUser { id } <- SU.fromJust <$> (SDU.userBy $ Email email)
                        suggestion <- SD.scalar' (Query "select count(1) from suggestions where suggested = $1") $ Row1 id
                        R.liftAff $ TUA.equal 1 suggestion






