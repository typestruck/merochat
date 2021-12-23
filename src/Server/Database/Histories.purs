module Server.Database.Histories where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Server.Database.Users (UsersTable)

type Histories =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , sender ∷ Column Int (ForeignKey "id" UsersTable)
      , recipient ∷ Column Int (ForeignKey "id" UsersTable)
      , first_message_date ∷ Column DateTime Default
      , date ∷ Column DateTime Default
      , sender_archived ∷ Column Boolean Default
      , recipient_archived ∷ Column Boolean Default
      )

histories ∷ Table "histories" Histories
histories = Table

_first_message_date ∷ Proxy "first_message_date"
_first_message_date = Proxy

_sender_archived ∷ Proxy "_sender_archived"
_sender_archived = Proxy