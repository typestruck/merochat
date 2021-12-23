module Server.Database.Tags where

import Droplet.Language

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))

type Tags =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      )

type TagsTable = Table "tags" Tags

tags ∷ TagsTable
tags = Table

_tags ∷ Proxy "tags"
_tags = Proxy
