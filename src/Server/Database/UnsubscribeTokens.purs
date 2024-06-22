module Server.Database.UnsubscribeTokens where

import Droplet.Language
import Prelude
import Prim hiding (Constraint)
import Data.Tuple.Nested (type (/\))

import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type UnsubscribeTokens =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , contents ∷ String
      , unsubscriber ∷ Column Int (Constraint "unsubcribe_tokens_user" (ForeignKey "id" UsersTable))
      )

unsubscribeTokens ∷ Table "unsubscribe_tokens" UnsubscribeTokens
unsubscribeTokens = Table

_unsubscriber ∷ Proxy "unsubscriber"
_unsubscriber = Proxy
