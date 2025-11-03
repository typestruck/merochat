module Server.KarmaPrivileges.Handler where

import Prelude
import Server.Effect

import Run as R
import Server.KarmaPrivileges.Action as SLA
import Server.KarmaPrivileges.Template as SLT
import Shared.Html (Html)

leaderboard ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
leaderboard { guards: { loggedUserId } } = do
      payload ← SLA.leaderboard loggedUserId
      R.liftEffect $ SLT.template payload
