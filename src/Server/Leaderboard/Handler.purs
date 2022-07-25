module Server.Leaderboard.Handler where

import Prelude
import Server.Types

import Run as R
import Server.Leaderboard.Action as SLA
import Server.Leaderboard.Template as SLT

leaderboard ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
leaderboard { guards: { loggedUserId } } = do
      payload <- SLA.leaderboard loggedUserId
      R.liftEffect $ SLT.template payload
