module Server.Landing.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.KarmaHistories
import Server.Database.LastSeen
import Server.Database.Suggestions
import Server.Database.Users
import Server.Effect

import Data.DateTime as DD
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple.Nested ((/\))
import Effect.Class as ER
import Effect.Now as EN
import Server.Database as SD
import Server.Database.Changelogs (_action, _changed, changelogs)
import Server.Database.Types (Checked(..))
import Shared.Changelog (ChangelogAction(..))
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
      now ← ER.liftEffect EN.nowDateTime
      threeDaysIn ← SU.fromJust <<< DD.adjust (Days 3.0) <$> ER.liftEffect EN.nowDateTime
      SD.withTransaction $ \connection → do
            userId ← _.id <<< SU.fromJust <$> (SD.singleWith connection (insert # into users (_name /\ _password /\ _email /\ _headline /\ _description /\ _temporary) # values (user.name /\ user.password /\ user.email /\ user.headline /\ user.description /\ Checked user.temporary) # returning _id))
            SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (50 /\ userId)
            SD.unsafeExecuteWith connection ("insert into karma_leaderboard(ranker, current_karma, gained, position) values (@ranker, 50, 0, ((select count(1) from karma_leaderboard) + 1))") { ranker: userId }
            SD.executeWith connection $ insert # into suggestions (_suggested /\ _score) # values (userId /\ 0)
            SD.executeWith connection $ insert # into last_seen (_who /\ _date) # values (userId /\ now)
            SD.executeWith connection $ insert # into changelogs (_changed /\ _description /\ _date /\ _action) # values (Just userId /\ "Support Merochat ❤️" /\ threeDaysIn /\ Just OpenBackerPage)
            pure userId