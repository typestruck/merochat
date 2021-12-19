module Server.Database.KarmaHistories where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type KarmaHistories =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , target ∷ Column Int (ForeignKey "id" UsersTable)
      , amount ∷ Int
      , date ∷ Column DateTime Default
      )

karma_histories ∷ Table "karma_histories" KarmaHistories
karma_histories = Table

_target ∷ Proxy "target"
_target = Proxy

_amount ∷ Proxy "amount"
_amount = Proxy
