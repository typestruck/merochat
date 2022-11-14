module Test.Server.User where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Users
import Server.Effect

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Server.Database as SD
import Server.Landing.Database (UserSignUp)
import Shared.Unsafe as SU

userCount ∷ ServerEffect Int
userCount = do
      count ← SD.single $ select (count _id # as c) # from users
      pure $ case count of
            Just { c } → SU.fromJust $ DB.toInt c
            Nothing → 0

baseUser ∷ UserSignUp
baseUser =
      { email: Just email
      , name: "Name"
      , password: Just password
      , headline: "headline"
      , description: "description"
      , temporary: false
      }

email ∷ String
email = "e@a.com"

password ∷ String
password = "hunter12"