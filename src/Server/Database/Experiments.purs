module Server.Database.Experiments where

import Droplet.Language

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Shared.Experiments.Types (ExperimentData)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type Experiments =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , code ∷ ExperimentData
      , name ∷ String
      , description ∷ String
      , added ∷ Column DateTime Default
      )

experiments ∷ Table "experiments" Experiments
experiments = Table

_description ∷ Proxy "description"
_description = Proxy

_added ∷ Proxy "added"
_added = Proxy

_code ∷ Proxy "code"
_code = Proxy
