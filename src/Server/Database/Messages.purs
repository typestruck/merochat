module Server.Database.Messages where

import Droplet.Language
import Shared.ContentType
import Shared.DateTime
import Shared.Im.Types
import Prim hiding (Constraint)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Messages =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , temporary_id ∷ Int
      , sender ∷ Column Int (Constraint "from_user_message" (ForeignKey "id" UsersTable))
      , recipient ∷ Column Int (Constraint "to_user_message" (ForeignKey "id" UsersTable))
      , date ∷ Column DateTimeWrapper Default
      , content ∷ String
      , status ∷ Column MessageStatus Default
      , visualized ∷ Maybe DateTime
      )

messages ∷ Table "messages" Messages
messages = Table

_temporary_id ∷ Proxy "temporary_id"
_temporary_id = Proxy

_content ∷ Proxy "content"
_content = Proxy

_status ∷ Proxy "status"
_status = Proxy

_visualized ∷ Proxy "visualized"
_visualized = Proxy
