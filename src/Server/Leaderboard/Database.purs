module Server.Leaderboard.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Fields
import Server.Database.KarmaLeaderboard
import Server.Database.Users
import Server.Types
import Shared.Leaderboard.Types

import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

fetchTop10 ∷ ServerEffect (Array LeaderboardUser)
fetchTop10 = SD.query $ select (((u ... _name) # as _name) /\ ((u ... _avatar) # as _avatar) /\ ((k ... _position) # as _position) /\ ((k ... _current_karma) # as _karma)) # from (((karma_leaderboard # as k) `join` (users # as u)) # on (k ... _ranker .=. u ... _id)) # orderBy (k ... _id) # limit 10

_karma ∷ Proxy "karma"
_karma = Proxy

userPosition ∷ Int → ServerEffect Int
userPosition loggedUserID = _.position <<< SU.fromJust <$> (SD.single $ select _position # from karma_leaderboard # wher (_ranker .=. loggedUserID))

fetchInBetween10 ∷ Int → ServerEffect (Array LeaderboardUser)
fetchInBetween10 position = SD.unsafeQuery "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id where position between greatest(1, @position - 5) and @position + 5 order by position" { position }