module Server.Database.DoppelgangerQuestions where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type DoppelgangerQuestions =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , question ∷ String
      )

doppelganger_questions ∷ Table "doppelganger_questions" DoppelgangerQuestions
doppelganger_questions = Table

_question ∷ Proxy "question"
_question = Proxy
