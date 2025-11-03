module Server.Logout.Handler where

import Prelude
import Server.Effect

import Payload.ResponseTypes (Empty(..), Response)
import Server.Logout as SL
import Server.Logout.Action as SLA
import Shared.Routes (routes)

logout ∷ { guards ∷ { loggedUserId ∷ Int, loggedUserToken ∷ String } } → ServerEffect (Response Empty)
logout request = do
      SLA.logout request.guards.loggedUserId request.guards.loggedUserToken
      pure $ SL.logout (routes.login.get {}) Empty
