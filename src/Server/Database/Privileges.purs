module Server.Database.Privileges where

import Droplet.Language
import Prelude

import Data.Maybe as DM
import Data.Tuple.Nested (type (/\))
import Server.Database as SD
import Server.Database.Fields (c)
import Server.Types (ServerEffect)
import Shared.Privilege (Privilege)
import Type.Proxy (Proxy(..))

type Privileges =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , feature ∷ Privilege
      , description ∷ String
      , quantity ∷ Int
      )

type PrivilegesTable =Table "privileges" Privileges

privileges ∷ PrivilegesTable
privileges = Table

_feature ∷ Proxy "feature"
_feature = Proxy

_quantity ∷ Proxy "quantity"
_quantity = Proxy

_privileges :: Proxy "privileges"
_privileges = Proxy

hasPrivilege :: Int -> Privilege -> ServerEffect Boolean
hasPrivilege loggedUserId p = do
      record <- SD.single $ select (1 # as c) # from privileges # wher (_feature .=. p)
      pure $ DM.isJust record