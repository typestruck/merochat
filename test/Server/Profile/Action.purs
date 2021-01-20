module Test.Server.Profile.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Query(..), Row1(..))
import Run as R
import Server.Database as SD
import Server.Landing.Database as SLD
import Server.Profile.Action as SPA
import Shared.Options.File (imageBasePath)
import Test.Server as TS
import Test.Server.Model (baseUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "landing actions" do
            TU.test "saveProfile ignores current picture" $
                  TS.serverAction $ do
                        userID <- SLD.createUser $ baseUser { email = "b@b.com" }
                        void $ SPA.saveProfile userID { id: userID, karma:0, karmaPosition:0, gender: Nothing, country: Nothing, name:"a", age: Nothing, headline:"a", description:"a", avatar: Just $ imageBasePath <> "upload/avatar.png", languages:[], tags:[] }
                        avatar <- SD.scalar' (Query "select avatar from users where id = $1") $ Row1 userID
                        R.liftAff $ TUA.equal (Just "avatar.png") avatar

