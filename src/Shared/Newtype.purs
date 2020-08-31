module Shared.Newtype where

import Prelude
import Shared.Types

import Data.Newtype (class Newtype)
import Data.Newtype as DN

unwrapAll :: forall f g v w. Functor f => Functor g => Newtype w v => f (g w) -> f (g v)
unwrapAll = map (map DN.unwrap)