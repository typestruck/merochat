module Server.Logout.Action where

import Prelude

import Server.Effect (ServerEffect)
import Server.Logout.Database as SLD

logout ∷ Int → String → ServerEffect Unit
logout loggedUserId loggedUserToken = SLD.deleteToken loggedUserId loggedUserToken