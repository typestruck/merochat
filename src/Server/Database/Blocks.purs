module Server.Database.Blocks where

import Type.Proxy (Proxy(..))
import Droplet.Language

type Blocks = (
      id :: Auto Int,
      blocker :: Int,
      blocked :: Int
)

blocks :: Table "blocks" Blocks
blocks = Table

_id :: Proxy "id"
_id = Proxy

_blocker :: Proxy "blocker"
_blocker = Proxy

_blocked :: Proxy "blocked"
_blocked = Proxy
