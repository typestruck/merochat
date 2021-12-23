module Server.Database.Recoveries where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Recoveries =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , uuid ∷ String
      , created ∷ Column DateTime Default
      , active ∷ Column Boolean Default
      , recoverer ∷ Column Int (ForeignKey "id" UsersTable)
      )

recoveries ∷ Table "recoveries" Recoveries
recoveries = Table

_uuid ∷ Proxy "uuid"
_uuid = Proxy

_created ∷ Proxy "created"
_created = Proxy

_recoverer ∷ Proxy "recoverer"
_recoverer = Proxy
