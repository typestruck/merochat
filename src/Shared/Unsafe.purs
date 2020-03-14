module Shared.Unsafe where

import Prelude
import Partial.Unsafe as PU
import Data.Array as DA
import Data.Maybe as DM
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe as EEU

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex array i = unsafeFromJust "unsafeIndex" $ DA.index array i

infixl 8 unsafeIndex as !@

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust message =
        case _ of
                Nothing -> EEU.unsafeThrow $ message <> " failed! received Nothing"
                Just j -> j