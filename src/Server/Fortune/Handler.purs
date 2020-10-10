module  Server.Fortune.Handler where

import Prelude
import Server.Types
import Shared.Types

import Server.Fortune.Action as SFA

fortune :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect String
fortune _ = SFA.fortunate
