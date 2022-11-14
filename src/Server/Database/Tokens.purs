module Server.Database.Tokens where

import Droplet.Language
import Prelude
import Prim hiding (Constraint)

import Data.Maybe as DM
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Fields (_contents)
import Server.Database.Users (UsersTable)
import Server.Effect (BaseEffect)
import Type.Proxy (Proxy(..))

type Tokens =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , contents ∷ String
      , toker ∷ Column Int (Constraint "tokens_user_user" (ForeignKey "id" UsersTable))
      )

tokens ∷ Table "tokens" Tokens
tokens = Table

_toker ∷ Proxy "toker"
_toker = Proxy

insertToken ∷ forall r. Int → String → BaseEffect { pool ∷ Pool | r } Unit
insertToken id contents = SD.unsafeExecute "INSERT INTO tokens (toker, contents) VALUES (@id, @contents) ON CONFLICT (toker) DO UPDATE SET contents = @contents" {id, contents}

tokenExists ∷ forall r. Int → String → BaseEffect { pool ∷ Pool | r } Boolean
tokenExists id contents = map DM.isJust <<< SD.single $ select (1 # as _toker) # from tokens # wher (_contents .=. contents .&&. _toker .=. id)