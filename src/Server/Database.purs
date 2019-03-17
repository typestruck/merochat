module Server.Database where

import Prelude

import Database.PostgreSQL(Query(..), Pool(..))
import Database.PostgreSQL as P
import Database.PostgreSQL.Row
import Data.Decimal as S
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

pool ∷ Aff Pool
pool = P.newPool $ (P.defaultPoolConfiguration "melanchat") { idleTimeoutMillis = Just 1000 }

