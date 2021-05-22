module Server.Landing.Database where

import Server.Types
import Shared.Types
import Prelude
import Server.Database as SD
import Shared.Unsafe as SU
import Data.Maybe(Maybe)
import Data.Tuple.Nested ((/\))
import Droplet.Language
import Server.Database.KarmaHistories

createUser :: { email :: String, name :: String, password :: String, headline :: String, description :: String } -> ServerEffect Int
createUser user = SD.withTransaction $ \connection -> do
      userID <- _.id  <<< SU.fromJust <$> (SD.unsafeSingleWith connection ("""INSERT INTO users(name, password, email, headline, description) VALUES (@name, @password, @email, @headline, @description) returning id""") user :: DatabaseEffect (Maybe { id :: Int }))
      SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (5 /\ userID)
      SD.unsafeExecuteWith connection ("insert into karma_leaderboard(ranker, current_karma, gained, position) values (@ranker, 5, 0, ((select count(1) from karma_leaderboard) + 1))") {ranker : userID }
      --use the median score as new user suggestion score so they are not thrown to the bottom of the pile
      SD.unsafeExecuteWith connection ("insert into suggestions(suggested, score) values (@suggested, coalesce((select score from suggestions order by id limit 1 offset ((select count(*) from suggestions) / 2)), 0))") { suggested: userID }
      pure userID