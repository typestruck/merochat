module Server.Database.Suggestions where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)

type Suggestions =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , suggested ∷ Column Int (ForeignKey "id" UsersTable)
      , bin ∷ Column Int Default
      , score ∷ Int
      )

suggestions ∷ Table "suggestions" Suggestions
suggestions = Table

_suggested ∷ Proxy "suggested"
_suggested = Proxy

_score ∷ Proxy "score"
_score = Proxy

_bin ∷ Proxy "bin"
_bin = Proxy
