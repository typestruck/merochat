module Server.Database.DoppelgangerChoices where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type DoppelgangerChoices =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , choice ∷ String
      , asked ∷ Int
      )

doppelganger_choices ∷ Table "doppelganger_choices" DoppelgangerChoices
doppelganger_choices = Table

_choice ∷ Proxy "choice"
_choice = Proxy

_asked ∷ Proxy "asked"
_asked = Proxy

