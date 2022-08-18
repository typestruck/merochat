module Server.Database.Flat where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS

splitAgg ∷ String → Maybe String → Array String
splitAgg pattern = DM.maybe [] (DS.split (Pattern pattern))