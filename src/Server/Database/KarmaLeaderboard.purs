module Server.Database.KarmaLeaderboard where

import Droplet.Language
import Prelude

import Data.DateTime (DateTime)
import Data.Maybe as DM
import Data.Tuple.Nested (type (/\))
import Droplet.Driver (Configuration, Pool)
import Server.Database as SD
import Server.Database.Users (UsersTable)
import Server.Effect (ServerEffect, BaseEffect)
import Type.Proxy (Proxy(..))

type KarmaLeaderboard =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , ranker ∷ Column Int (ForeignKey "id" UsersTable /\ Unique)
      , position ∷ Int
      , current_karma ∷ Int
      , gained ∷ Int
      , date ∷ Column DateTime Default
      )

karma_leaderboard ∷ Table "karma_leaderboard" KarmaLeaderboard
karma_leaderboard = Table

_ranker ∷ Proxy "ranker"
_ranker = Proxy

_position ∷ Proxy "position"
_position = Proxy

_current_karma ∷ Proxy "current_karma"
_current_karma = Proxy

_gained ∷ Proxy "gained"
_gained = Proxy

_karma ∷ Proxy "karma"
_karma = Proxy

_karmaPosition ∷ Proxy "karmaPosition"
_karmaPosition = Proxy

fetchKarma ∷ ∀ r. Int → BaseEffect { pool ∷ Pool | r } Int
fetchKarma loggedUserId = map (DM.maybe 0 _.current_karma) <<< SD.single $ select _current_karma # from karma_leaderboard # wher (_ranker .=. loggedUserId)
