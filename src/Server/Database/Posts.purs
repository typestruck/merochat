module Server.Database.Posts where

import Droplet.Language
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Fields (_date, _id)
import Server.Database.Messages (_content)
import Server.Effect (BaseEffect)
import Shared.DateTime (DateTimeWrapper)
import Shared.Post (Post)
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

fetchPost ∷ ∀ r. Int → BaseEffect { pool ∷ Pool | r } (Maybe Post)
fetchPost id = SD.single $ select (_id /\ _content /\ _date /\ _expires) # from posts # wher (_id .=. id)