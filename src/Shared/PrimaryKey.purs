module Shared.PrimaryKey where

import Shared.Types 
import Data.Int53 as DI
import Prelude

fromInt :: Int -> PrimaryKey
fromInt = PrimaryKey <<< DI.fromInt 