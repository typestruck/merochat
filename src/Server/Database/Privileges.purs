module Server.Im.Database.Privileges where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type Privileges =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , feature ∷ Int
      , description ∷ String
      , quantity ∷ Int
      )

type PrivilegesTable =Table "privileges" Privileges

privileges ∷ PrivilegesTable
privileges = Table

_feature ∷ Proxy "feature"
_feature = Proxy

_quantity ∷ Proxy "quantity"
_quantity = Proxy

_privileges :: Proxy "privileges"
_privileges = Proxy
