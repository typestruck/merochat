module Server.Logout.Handler where

import Prelude
import Server.Effect

import Payload.ResponseTypes (Response)
import Server.Logout as SL
import Shared.Routes (routes)
import Server.Logout.Action as SLA
import Server.Ok

logout ∷ { guards ∷ { loggedUserId ∷ Int, loggedUserToken ∷ String } } → ServerEffect (Response Ok)
logout request = do
      SLA.logout request.guards.loggedUserId request.guards.loggedUserToken
      pure $ SL.logout (routes.login.get {}) ok
