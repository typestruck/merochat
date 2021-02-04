module Server.Leaderboard.Database where

import Prelude
import Server.Types
import Shared.Types

import Database.PostgreSQL (Query(..), Row0(..), Row1(..))
import Server.Database as SD

fetchTop10 :: ServerEffect (Array LeaderboardUserWrapper)
fetchTop10 = SD.select (Query "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id order by k.id limit 10") Row0

userPosition :: PrimaryKey -> ServerEffect Int
userPosition loggedUserID = SD.scalar' (Query "select position from karma_leaderboard where ranker = $1") $ Row1 loggedUserID

fetchInBetween10 :: Int -> ServerEffect (Array LeaderboardUserWrapper)
fetchInBetween10 position = SD.select (Query "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id where position between greatest(1, $1 - 5) and $1 + 5 order by position") $ Row1 position