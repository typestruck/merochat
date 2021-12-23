module Server.Database.Blocks where

import Droplet.Language
import Prim hiding (Constraint)

import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Blocks =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , blocker ∷ Column Int (Constraint "blocker_user" (ForeignKey "id" UsersTable))
      , blocked ∷ Column Int (Constraint "blocked_user" (ForeignKey "id" UsersTable))
      )

blocks ∷ Table "blocks" Blocks
blocks = Table

_blocker ∷ Proxy "blocker"
_blocker = Proxy

_blocked ∷ Proxy "blocked"
_blocked = Proxy
