module Server.Landing.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.KarmaHistories
import Server.Database.Users
import Server.Types

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
createUser user = SD.withTransaction $ \connection → do
      userId ← _.id <<< SU.fromJust <$> (SD.singleWith connection (insert # into users (_name /\ _password /\ _email /\ _headline /\ _description /\ _temporary) # values (user.name /\ user.password /\ user.email /\ user.headline /\ user.description /\ Checked user.temporary) # returning _id))
      SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (5 /\ userId)
      SD.unsafeExecuteWith connection ("insert into karma_leaderboard(ranker, current_karma, gained, position) values (@ranker, 5, 0, ((select count(1) from karma_leaderboard) + 1))") { ranker: userId }
      --use the median score as new user suggestion score so they are not thrown to the bottom of the pile
      SD.unsafeExecuteWith connection ("insert into suggestions(suggested, score) values (@suggested, coalesce((select score from suggestions order by id limit 1 offset ((select count(*) from suggestions) / 2)), 0))") { suggested: userId }
      pure userId