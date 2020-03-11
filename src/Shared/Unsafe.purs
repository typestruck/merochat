module Shared.Unsafe where

import Prelude
import Partial.Unsafe as PU
import Data.Array as DA
import Data.Maybe as DM
import Data.Maybe (Maybe(..))

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex array i = unsafeFromJust $ DA.index array i

infixl 8 unsafeIndex as !@

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust just = PU.unsafePartial $ DM.fromJust just