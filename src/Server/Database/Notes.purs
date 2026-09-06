module Server.Database.Notes where

import Droplet.Language
import Prim hiding (Constraint)

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Server.Database.Fields (_id)
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Notes =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , author ∷ Column Int (Constraint "author_user" (ForeignKey "id" UsersTable))
      , target ∷ Column Int (Constraint "target_user" (ForeignKey "id" UsersTable))
      , content ∷ String
      , date ∷ Column DateTime Default
      )

notes ∷ Table "notes" Notes
notes = Table

_author ∷ Proxy "author"
_author = Proxy

_target ∷ Proxy "target"
_target = Proxy