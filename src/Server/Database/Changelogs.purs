module Server.Database.Changelogs where

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language (Column, Default, ForeignKey, Identity, PrimaryKey, Table(..))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Changelogs =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , changed ∷ Maybe Int
      , description ∷ String
      , date :: Column DateTime Default
      )

type ChangelogsTable = Table "changelogs" Changelogs

changelogs ∷ ChangelogsTable
changelogs = Table

_changed ∷ Proxy "changed"
_changed = Proxy

