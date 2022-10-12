module Server.KarmaPrivileges.Types where

import Prelude

import Shared.KarmaPrivileges.Types (LeaderboardUser, PrivilegeUser)

type Payload =
      { top10 ∷ Array LeaderboardUser
      , userPosition ∷ Int
      , inBetween10 ∷ Array LeaderboardUser
      , privileges ∷ Array PrivilegeUser
      }