module Server.Database.Posts where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language (Column, Default, Identity, PrimaryKey, Table(..))
import Shared.DateTime (DateTimeWrapper)
import Type.Proxy (Proxy(..))

type Posts =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , expires ∷ Maybe DateTimeWrapper
      , poster ∷ Int
      , content ∷ String
      , date ∷ Column DateTimeWrapper Default
      )

type PostsTable = Table "posts" Posts

posts ∷ PostsTable
posts = Table

_expires ∷ Proxy "expires"
_expires = Proxy

_poster ∷ Proxy "poster"
_poster = Proxy

_totalPosts ∷ Proxy "totalPosts"
_totalPosts = Proxy

_unseenPosts ∷ Proxy "unseenPosts"
_unseenPosts = Proxy