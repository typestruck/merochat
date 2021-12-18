module Server.Database.KarmaLeaderboard where

import Droplet.Language
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Data.DateTime (DateTime)
import Server.Database.Users (UsersTable)

type KarmaLeaderboard =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , ranker ∷ Column Int (ForeignKey "id" UsersTable)
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