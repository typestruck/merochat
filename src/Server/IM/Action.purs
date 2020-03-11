module Server.IM.Action where

import Prelude
import Shared.Types

import Server.Types
import Server.IM.Database as SID

suggest :: PrimaryKey -> ServerEffect (Array IMUser)
suggest id = SID.suggest id