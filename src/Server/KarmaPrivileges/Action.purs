module Server.KarmaPrivileges.Action where

import Prelude

import Server.KarmaPrivileges.Database as SLD
import Server.KarmaPrivileges.Types (Payload)
import Server.Types (ServerEffect)

leaderboard :: Int -> ServerEffect Payload
leaderboard loggedUserId = do
      top10 ← SLD.fetchTop10
      userPosition ← SLD.userPosition loggedUserId
      inBetween10 ← SLD.fetchInBetween10 userPosition
      pure { top10, inBetween10, userPosition }