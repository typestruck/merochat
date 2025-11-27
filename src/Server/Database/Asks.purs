module Server.Database.Asks where

import Droplet.Language
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Fields (_date, _id)
import Server.Database.Messages (_content)
import Server.Effect (BaseEffect)
import Shared.DateTime (DateTimeWrapper)
import Shared.Post (Post)
import Type.Proxy (Proxy(..))

type Asks =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , asker ∷ Int
      , answerer ∷ Int
      , question :: String
      , answer :: Maybe String
      , date ∷ Column DateTimeWrapper Default
      )

type AsksTable = Table "asks" Asks

asks ∷ AsksTable
asks = Table

_answerer ∷ Proxy "answerer"
_answerer = Proxy

_answer ∷ Proxy "answer"
_answer = Proxy

_totalAsks ∷ Proxy "totalAsks"
_totalAsks = Proxy