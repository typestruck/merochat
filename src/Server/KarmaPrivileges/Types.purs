module Server.KarmaPrivileges.Types where

import Prelude

import Shared.KarmaPrivileges.Types (LeaderboardUser)

type Payload = { top10 ∷ Array LeaderboardUser, userPosition ∷ Int, inBetween10 ∷ Array LeaderboardUser }