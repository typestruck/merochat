module Server.KarmaPrivileges.Handler where

import Prelude
import Server.Types

import Run as R
import Server.KarmaPrivileges.Action as SLA
import Server.KarmaPrivileges.Template as SLT

leaderboard ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
leaderboard { guards: { loggedUserId } } = do
      payload <- SLA.leaderboard loggedUserId
      R.liftEffect $ SLT.template payload
