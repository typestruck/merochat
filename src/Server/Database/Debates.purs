module Server.Database.Debates where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Server.Database.Types (Checked(..))
import Type.Proxy (Proxy(..))

type Debates =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , topic ∷ String
      , created ∷ Column DateTime Default
      , ongoing ∷ Column Checked Default
      , pro ∷ Maybe Int
      , con ∷ Maybe Int
      , is_public ∷ Column Checked Default
      )

debates ∷ Table "debates" Debates
debates = Table

_topic ∷ Proxy "topic"
_topic = Proxy

_pro ∷ Proxy "pro"
_pro = Proxy

_con ∷ Proxy "con"
_con = Proxy

_isPublic ∷ Proxy "is_public"
_isPublic = Proxy

_ongoing ∷ Proxy "ongoing"
_ongoing = Proxy