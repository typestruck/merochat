module Server.Landing.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.KarmaHistories
import Server.Database.LastSeen
import Server.Database.Suggestions
import Server.Database.Users
import Server.Effect

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Types (Checked(..))
import Shared.Unsafe as SU

type UserSignUp =
      { email ∷ Maybe String
      , name ∷ String
      , password ∷ Maybe String
      , headline ∷ String
      , description ∷ String
      , temporary ∷ Boolean
      }

--refactor: add support on droplet
createUser ∷ UserSignUp → ServerEffect Int
createUser user = do
      SD.withTransaction $ \connection → do
            userId ← _.id <<< SU.fromJust <$> (SD.singleWith connection (insert # into users (_name /\ _password /\ _email /\ _headline /\ _description /\ _temporary) # values (user.name /\ user.password /\ user.email /\ user.headline /\ user.description /\ Checked user.temporary) # returning _id))
            SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (50 /\ userId)
            SD.unsafeExecuteWith connection ("insert into karma_leaderboard(ranker, current_karma, gained, position) values (@ranker, 50, 0, ((select count(1) from karma_leaderboard) + 1))") { ranker: userId }
            SD.executeWith connection $ insert # into suggestions (_suggested /\ _score) # values (userId /\ 0)
            pure userId