module Server.Database.Posts where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.KarmaLeaderboard

import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Tuple.Nested (type (/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Fields (c)
import Server.Effect (ServerEffect, BaseEffect)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Privilege (Privilege)
import Type.Proxy (Proxy(..))

type Posts =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , expires ∷ Maybe DateTimeWrapper
      , poster ∷ Int
      , content ∷ String
      , date ∷ DateTimeWrapper
      )

type PostsTable = Table "posts" Posts

posts ∷ PostsTable
posts = Table

_expires ∷ Proxy "expires"
_expires = Proxy

_poster ∷ Proxy "poster"
_poster = Proxy

_totalPosts :: Proxy "totalPosts"
_totalPosts = Proxy