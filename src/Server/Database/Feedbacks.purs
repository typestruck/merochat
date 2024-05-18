module Server.Database.Feedbacks where

import Droplet.Language

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Feedbacks =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , comments ∷ String
      , file_name ∷ Maybe String
      , feedbacker ∷ Column Int (ForeignKey "id" UsersTable)
      )

feedbacks ∷ Table "feedbacks" Feedbacks
feedbacks = Table

_comments ∷ Proxy "comments"
_comments = Proxy

_fileName ∷ Proxy "file_name"
_fileName = Proxy

_feedbacker ∷ Proxy "feedbacker"
_feedbacker = Proxy
