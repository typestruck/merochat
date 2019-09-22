module Server.IM.Action where

import Prelude
import Shared.Types

import Data.Int53 (Int53)
import Server.Types (ServerEffect)

suggest :: Int53 -> ServerEffect (Array IMUser)
suggest id = pure []