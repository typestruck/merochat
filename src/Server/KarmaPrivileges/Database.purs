module Server.KarmaPrivileges.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Fields
import Server.Database.Histories
import Server.Database.KarmaLeaderboard
import Server.Database.Messages
import Server.Database.Users
import Server.Types
import Shared.KarmaPrivileges.Types

import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Shared.Avatar as SA
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

fetchTop10 ∷ ServerEffect (Array LeaderboardUser)
fetchTop10 = avatarPath <$> (SD.query $ select (((u ... _name) # as _name) /\ ((u ... _avatar) # as _avatar) /\ ((k ... _position) # as _position) /\ ((k ... _current_karma) # as _karma)) # from (((karma_leaderboard # as k) `join` (users # as u)) # on (k ... _ranker .=. u ... _id)) # orderBy (k ... _id) # limit (Proxy ∷ _ 10))

userPosition ∷ Int → ServerEffect Int
userPosition loggedUserId = _.position <<< SU.fromJust <$> (SD.single $ select _position # from karma_leaderboard # wher (_ranker .=. loggedUserId))

--refactor: add greatest to droplet
fetchInBetween10 ∷ Int → ServerEffect (Array LeaderboardUser)
fetchInBetween10 position = avatarPath <$> SD.unsafeQuery "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id where position between greatest(1, @position - 5) and @position + 5 order by position" { position }

fetchPrivileges ∷ Int → ServerEffect (Array PrivilegeUser)
fetchPrivileges loggedUserId = SD.unsafeQuery "SELECT name, description, quantity, current_karma >= quantity AS got FROM privileges LEFT JOIN karma_leaderboard ON ranker = @loggedUserId ORDER BY quantity desc" { loggedUserId }

fetchStats ∷ Int → ServerEffect _
fetchStats loggedUserId = map (ks <<< SU.fromJust) <<< SD.single $
      select (
            (select (_current_karma # as _karma) # from karma_leaderboard # wher (_ranker .=. loggedUserId) # orderBy _id # limit (Proxy :: Proxy 1)) /\
            (select (count _id # as _sent) # from messages # wher (_sender .=. loggedUserId) # orderBy _sent # limit (Proxy :: Proxy 1)) /\
            (select (count _id # as _started) # from histories # wher (_sender .=. loggedUserId) # orderBy _started # limit (Proxy :: Proxy 1)) /\
            (select (count _id # as _total) # from histories # wher (_sender .=. loggedUserId .||. _recipient .=. loggedUserId) # orderBy _total # limit (Proxy :: Proxy 1))
      )
      where
      _sent = Proxy ∷ Proxy "sent"
      _started = Proxy ∷ Proxy "started"
      _total = Proxy ∷ Proxy "total"

      ks r = r
            { started = DM.fromMaybe 0 (r.started >>= DBI.toInt)
            , sent = DM.fromMaybe 0 (r.sent >>= DBI.toInt)
            , total = DM.fromMaybe 0 (r.total >>= DBI.toInt)
            , karma = DM.fromMaybe 0 r.karma
            }

avatarPath = map $ \u → u { avatar = SA.parseAvatar u.avatar }