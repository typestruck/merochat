module Server.Database.Messages where

import Type.Proxy (Proxy(..))
import Droplet.Language
import Data.DateTime (DateTime)
import Shared.IM.Types
import Shared.DateTime
import Shared.ContentType
import Data.Maybe (Maybe(..))

type Messages =
      ( id ∷ Auto Int
      , temporary_id ∷ Int
      , sender ∷ Int
      , recipient ∷ Int
      , date ∷ Default DateTimeWrapper
      , content ∷ String
      , status ∷ Default MessageStatus
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
