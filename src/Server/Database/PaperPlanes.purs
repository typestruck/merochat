module Server.Database.PaperPlanes where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Shared.Experiments.Types (PlaperPlaneStatus)
import Type.Proxy (Proxy(..))

type PaperPlanes =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , thrower ∷ Int
      , message ∷ String
      , created ∷ Column DateTime Default
      , by_at :: Maybe DateTime
      , by :: Maybe Int
      , status ∷ PlaperPlaneStatus
      )

paper_planes ∷ Table "paper_planes" PaperPlanes
paper_planes = Table

_thrower ∷ Proxy "thrower"
_thrower = Proxy

_message ∷ Proxy "message"
_message = Proxy

_by ∷ Proxy "by"
_by = Proxy

_byAt ∷ Proxy "by_at"
_byAt = Proxy
