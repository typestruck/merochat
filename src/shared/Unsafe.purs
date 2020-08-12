module Shared.Unsafe where

import Prelude
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe as EEU

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex array i = fromJust $ DA.index array i

infixl 8 unsafeIndex as !@

-- | This function should be used when a Nothing value is a programming error.
fromJust :: forall a. Maybe a -> a
fromJust =
      case _ of
            Nothing -> EEU.unsafeThrow $ "fromJust failed! received Nothing"
            Just j -> j