module Server.Database.Reports where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Shared.IM.Types (ReportReason)
import Type.Proxy (Proxy(..))

type Reports =
      ( id ∷ Auto Int
      , comment ∷ Maybe String
      , reason ∷ ReportReason
      , reporter ∷ Int
      , date ∷ Default DateTime
      , reported ∷ Int
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
