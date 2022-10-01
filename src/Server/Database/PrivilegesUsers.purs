module Server.Im.Database.PrivilegesUsers where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Server.Im.Database.Privileges (PrivilegesTable)
import Type.Proxy (Proxy(..))

type Privileges =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , privilege ∷ Column Int (ForeignKey "id" PrivilegesTable)
      , receiver ∷ Column Int (ForeignKey "id" UsersTable)
      )

privileges_users ∷ Table "privileges_users" Privileges
privileges_users = Table

_privilege ∷ Proxy "privilege"
_privilege = Proxy

_receiver :: Proxy "receiver"
_receiver = Proxy
