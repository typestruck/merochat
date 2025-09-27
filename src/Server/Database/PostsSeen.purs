module Server.Database.PostsSeen where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language (Column, Default, Identity, PrimaryKey, Table(..))
import Shared.DateTime (DateTimeWrapper)
import Type.Proxy (Proxy(..))

type Posts =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , until :: Int
      , poster ∷ Int
      , reader ∷ Int
      )

type PostsTable = Table "posts_seen" Posts

posts_seen ∷ PostsTable
posts_seen = Table

_reader ∷ Proxy "reader"
_reader = Proxy

_until ∷ Proxy "until"
_until = Proxy