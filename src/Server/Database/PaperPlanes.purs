module Server.Database.PaperPlanes where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Shared.Experiments.Types (PlaperPlaneStatus)
import Type.Proxy (Proxy(..))

type PaperPlanes =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , thrower ∷ Int
      , message ∷ String
      , date ∷ Column DateTime Default
      , status ∷ PlaperPlaneStatus
      )

paper_planes ∷ Table "paper_planes" PaperPlanes
paper_planes = Table

_thrower ∷ Proxy "thrower"
_thrower = Proxy

_message ∷ Proxy "message"
_message = Proxy
