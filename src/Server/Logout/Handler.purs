module Server.Logout.Handler where

import Prelude
import Server.Types

import Payload.ResponseTypes (Response)
import Server.Logout as SL
import Server.Ok
import Shared.Routes (routes)

logout ∷ ∀ r. { | r } → ServerEffect (Response Ok)
logout _ = pure $ SL.logout (routes.login.get {}) ok
