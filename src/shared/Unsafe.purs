module Shared.Unsafe where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
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

-- | This function should be used when a Right value is a programming error.
fromRight :: forall a b. Either a b -> b
fromRight =
      case _ of
            Left _ -> EEU.unsafeThrow $ "fromRight failed! received Left"
            Right r -> r

toEnum :: forall a. BoundedEnum a => Int -> a
toEnum = fromJust <<< DE.toEnum