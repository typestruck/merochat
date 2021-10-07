module Server.Fortune.Handler where

import Prelude
import Server.Types
import Shared.ContentType

import Server.Fortune.Action as SFA

fortune ∷ { guards ∷ { loggedUserID ∷ Int } } → ServerEffect String
fortune _ = SFA.fortunate
