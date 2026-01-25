module Test.Server.User where

import Droplet.Language (as, count, from, select, wher, (.=.))
import Prelude
import Server.Database.Fields (_id, _name, c)
import Server.Database.Users
import Server.Effect (ServerEffect)

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Landing.Database (UserSignUp)
import Server.Landing.Database as SLD
import Shared.Unsafe as SU

userCount ∷ ServerEffect Int
userCount = do
      count ← SD.single $ select (count _id # as c) # from users
      pure $ case count of
            Just { c } → SU.fromJust $ DB.toInt c
            Nothing → 0

createUser ∷ ServerEffect Int
createUser = SLD.createUser baseUser

createUserWith ∷ (UserSignUp → UserSignUp) → ServerEffect Int
createUserWith f = SLD.createUser $ f baseUser

fetchUser ∷ Int → ServerEffect (Maybe _)
fetchUser id = SD.single $ select fields # from users # wher (_id .=. id)
      where
      fields = _id
            /\ _password
            /\ _name
            /\ _headline
            /\ _joined
            /\ _email
            /\ _birthday
            /\ _completedTutorial
            /\ _description
            /\ _avatar
            /\ _backer
            /\ _receiveEmail
            /\ _gender
            /\ _country
            /\ _readReceipts
            /\ _typingStatus
            /\ _temporary
            /\ _onlineStatus
            /\ _messageTimestamps
            /\ _chatBackground
            /\ _ownBackground
            /\ _visibility
            /\ _postsVisibility
            /\ _visibility_last_updated

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