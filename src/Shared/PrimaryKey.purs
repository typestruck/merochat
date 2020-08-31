module Shared.PrimaryKey where

import Prelude
import Shared.Types

import Data.Int53 as DI
import Data.Maybe (Maybe)

fromInt :: Int -> PrimaryKey
fromInt = PrimaryKey <<< DI.fromInt

fromString :: String -> Maybe PrimaryKey
fromString value = PrimaryKey <$> DI.fromString value