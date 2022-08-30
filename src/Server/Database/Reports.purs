module Server.Database.Reports where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Shared.Im.Types (ReportReason)
import Server.Database.Users (UsersTable)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type Reports =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , comment ∷ Maybe String
      , reason ∷ ReportReason
      , reporter ∷ Column Int (ForeignKey "id" UsersTable)
      , date ∷ Column DateTime Default
      , reported ∷ Column Int (ForeignKey "id" UsersTable)
      )

reports ∷ Table "reports" Reports
reports = Table

_comment ∷ Proxy "comment"
_comment = Proxy

_reason ∷ Proxy "reason"
_reason = Proxy

_reporter ∷ Proxy "reporter"
_reporter = Proxy

_reported ∷ Proxy "reported"
_reported = Proxy
