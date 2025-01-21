module Server.Database.Badges where

import Droplet.Language
import Data.Tuple.Nested (type (/\))

import Shared.Badge (Badge)
import Type.Proxy (Proxy(..))

type Badges =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , kind ∷ Int
      , description ∷ Badge
      )

type BadgesTable = Table "badges" Badges

badges ∷ BadgesTable
badges = Table

_kind ∷ Proxy "kind"
_kind = Proxy

_badges ∷ Proxy "badges"
_badges = Proxy
