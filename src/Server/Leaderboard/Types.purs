module Server.Leaderboard.Types where

import Prelude

import Shared.Leaderboard.Types (LeaderboardUser)

type Payload = { top10 ∷ Array LeaderboardUser, userPosition ∷ Int, inBetween10 ∷ Array LeaderboardUser }