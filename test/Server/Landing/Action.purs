module Test.Server.Landing.Action where

import Prelude
import Shared.Types

import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row0(..))
import Effect (Effect)
import Run as R
import Server.Landing.Action as SLA
import Server.Landing.Action (invalidUserEmailMessage)
import Server.Types (ServerEffect)
import Test.Server as TS
import Server.Database as SD
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

userCount :: ServerEffect Int
userCount = SD.scalar' (Query "select count(1) from users") Row0

tests :: Effect Unit
tests = TUM.runTest $ do
        TU.suite "landing actions" $ do
                TU.test "register" $ do
                        let catch (BadRequest {reason}) = R.liftAff $ TUA.equal invalidUserEmailMessage reason
                            catch other = R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other

                        TS.serverActionCatch catch $ \_ -> do
                                _ <- SLA.register "" RegisterLogin {
                                        email: "",
                                        password: "",
                                        captchaResponse: Nothing
                                }
                                users <- userCount
                                R.liftAff $ TUA.assert 0 users





