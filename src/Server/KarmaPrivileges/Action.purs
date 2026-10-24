module Server.KarmaPrivileges.Action where

import Prelude

import Server.KarmaPrivileges.Database as SLD
import Server.Effect (ServerEffect)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesData)

leaderboard ∷ Int → ServerEffect KarmaPrivilegesData
leaderboard loggedUserId = do
      top10 ← SLD.fetchTop10
      userPosition ← SLD.userPosition loggedUserId
      inBetween10 ← SLD.fetchInBetween10 userPosition
      privileges ← SLD.fetchPrivileges loggedUserId
      stats ← SLD.fetchStats loggedUserId
      pure { top10, inBetween10, userPosition, privileges, stats }