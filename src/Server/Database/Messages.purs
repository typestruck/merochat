module Server.Database.Messages where

import Droplet.Language
import Prim hiding (Constraint)
import Shared.DateTime
import Shared.Im.Types

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Server.Database.Types (Checked(..))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type Messages =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , sender ∷ Column Int (Constraint "from_user_message" (ForeignKey "id" UsersTable))
      , recipient ∷ Column Int (Constraint "to_user_message" (ForeignKey "id" UsersTable))
      , date ∷ Column DateTimeWrapper Default
      , content ∷ String
      , edited ∷ Column Checked Default
      , status ∷ Column MessageStatus Default
      , visualized ∷ Maybe DateTime
      )

messages ∷ Table "messages" Messages
messages = Table

_content ∷ Proxy "content"
_content = Proxy

_status ∷ Proxy "status"
_status = Proxy

_visualized ∷ Proxy "visualized"
_visualized = Proxy

_edited ∷ Proxy "edited"
_edited = Proxy

