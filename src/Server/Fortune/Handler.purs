module Server.Fortune.Handler where

import Prelude
import Server.Types

import Server.Fortune.Action as SFA

fortune ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
fortune _ = SFA.fortunate
