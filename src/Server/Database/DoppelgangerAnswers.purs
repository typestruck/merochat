module Server.Database.DoppelgangerAnswers where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type DoppelgangerAnswers =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , taker ∷ Int
      , choice ∷ Int
      )

doppelganger_answers ∷ Table "doppelganger_answers" DoppelgangerAnswers
doppelganger_answers = Table

_taker ∷ Proxy "taker"
_taker = Proxy
