module Server.Database.ChangelogRead where

import Data.Tuple.Nested (type (/\))
import Droplet.Language (Column, Identity, PrimaryKey, Table(..))
import Type.Proxy (Proxy(..))

type ChangelogRead =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , who ∷ Int
      , log ∷ Int
      )

type ChangelogReadTable = Table "changelog_read" ChangelogRead

changelog_read ∷ ChangelogReadTable
changelog_read = Table

_log ∷ Proxy "log"
_log = Proxy

