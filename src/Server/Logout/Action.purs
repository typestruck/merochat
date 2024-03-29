module Server.Logout.Action where

import Prelude

import Server.Effect (ServerEffect)
import Server.Logout.Database as SLD

logout ∷ Int → ServerEffect Unit
logout loggedUserId = SLD.logout loggedUserId