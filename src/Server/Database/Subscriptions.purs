module Server.Database.Subscriptions where

import Droplet.Language
import Prelude
import Prim hiding (Constraint)

import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Users (UsersTable)
import Server.Effect (BaseEffect)
import Type.Proxy (Proxy(..))

type Subscriptions =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , token ∷ String
      , subscriber ∷ Column Int (Constraint "sub_user" (ForeignKey "id" UsersTable))
      )

subscriptions ∷ Table "subscriptions" Subscriptions
subscriptions = Table

_token ∷ Proxy "token"
_token = Proxy

_subscriber ∷ Proxy "subscriber"
_subscriber = Proxy

fetchSubscriptions ∷ ∀ r. BaseEffect { pool ∷ Pool | r } (Array { subscriber ∷ Int, token ∷ String })
fetchSubscriptions = SD.query $ select (_subscriber /\ _token) # from subscriptions # orderBy _subscriber
