module Server.Database.Histories where

import Droplet.Language

import Data.DateTime (DateTime)
import Type.Proxy (Proxy(..))

type Histories =
      ( id :: Auto Int
      , sender :: Int
      , recipient :: Int
      , first_message_date :: Default DateTime
      , date :: Default DateTime
      , sender_archived :: Default Boolean
      , recipient_archived :: Default Boolean
      )

histories :: Table "histories" Histories
histories = Table

_first_message_date :: Proxy "first_message_date"
_first_message_date = Proxy

_sender_archived :: Proxy "_sender_archived"
_sender_archived = Proxy