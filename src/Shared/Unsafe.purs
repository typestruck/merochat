module Shared.Unsafe where

import Prelude
import Partial.Unsafe as PU
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe as EEU

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex array i = fromJust "unsafeIndex" $ DA.index array i

infixl 8 unsafeIndex as !@

-- | This function should be used when a Nothing value is a programming error.
fromJust :: forall a. String -> Maybe a -> a
fromJust message =
        case _ of
                Nothing -> EEU.unsafeThrow $ message <> " failed! received Nothing"
                Just j -> j