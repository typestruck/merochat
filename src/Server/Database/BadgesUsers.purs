module Server.Database.BadgesUsers where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Server.Database.Badges (BadgesTable)
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type BadgesUsers =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , receiver ∷ Column Int (ForeignKey "id" UsersTable)
      , badge ∷ Column Int (ForeignKey "id" BadgesTable)
      )

badges_users ∷ Table "badges_users" BadgesUsers
badges_users = Table

_receiver ∷ Proxy "receiver"
_receiver = Proxy

_badge ∷ Proxy "badge"
_badge = Proxy