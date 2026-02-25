module Server.Database.Praise where

import Data.Tuple.Nested (type (/\))
import Droplet.Language (Column, Default, Identity, PrimaryKey, Table(..))
import Shared.DateTime (DateTimeWrapper)
import Shared.Praise (PraisedFor)
import Type.Proxy (Proxy(..))

type Praise =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , praiser ∷ Int
      , praised ∷ Int
      , accepted :: Boolean
      , praised_for ∷ PraisedFor
      , date ∷ Column DateTimeWrapper Default
      )

type PraiseTable = Table "praises" Praise

praises ∷ PraiseTable
praises = Table

_praiser ∷ Proxy "praiser"
_praiser = Proxy

_praised ∷ Proxy "praised"
_praised = Proxy

_praised_for ∷ Proxy "praised_for"
_praised_for = Proxy

_accepted :: Proxy "accepted"
_accepted = Proxy