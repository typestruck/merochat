module Server.Logout.Handler where

import Prelude
import Server.Effect

import Payload.ResponseTypes (Response)
import Server.Logout as SL
import Shared.Routes (routes)
import Server.Logout.Action as SLA
import Server.Ok

logout ∷ ∀ r. { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Response Ok)
logout { guards: { loggedUserId } } = do
      SLA.logout loggedUserId
      pure $ SL.logout (routes.login.get {}) ok
