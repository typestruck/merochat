module Test.Server.Profile.Action where

import Prelude

import Data.Maybe (Maybe(..))

import Run as R
import Server.Database as SD
import Server.Landing.Database as SLD
import Server.Profile.Action as SPA
import Shared.Options.File (imageBasePath)
import Test.Server as TS
import Test.Server.Model (baseUser)
import Test.Unit (TestSuite)
import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Users
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Shared.User

tests ∷ TestSuite
tests = do
      TU.suite "profile actions" do
            TU.test "saveProfile ignores current picture" do
                  -- $ TS.serverAction
                  -- $ do
                  --       userId ← SLD.createUser $ baseUser { email = "b@b.com" }
                  --       void $ SPA.saveProfile userId
                  --             { id: userId
                  --             , karma: 0
                  --             , karmaPosition: 0
                  --             , availability: None
                  --             , gender: Nothing
                  --             , country: Nothing
                  --             , name: "a"
                  --             , age: Nothing
                  --             , headline: "a"
                  --             , description: "a"
                  --             , avatar: Just $ imageBasePath <> "upload/avatar.png"
                  --             , languages: []
                  --             , tags: []
                  --             }
                  --       avatar ← SD.single $ select _avatar # from users # wher (_id .=. userId)
                  --       R.liftAff $ TUA.equal (Just { avatar: Just "avatar.png" }) avatar
                  pure unit