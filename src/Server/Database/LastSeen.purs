module Server.Database.LastSeen where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type LastSeen =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , who ∷ Column Int (ForeignKey "id" UsersTable)
      , date ∷ DateTime
      )

last_seen ∷ Table "last_seen" LastSeen
last_seen = Table

_who ∷ Proxy "who"
_who = Proxy
