module Server.Database.Flat where

import Data.Maybe as DM
import Data.String as DS
import Data.String (Pattern(..))
import Prelude


splitAgg pattern = DM.maybe [] (DS.split (Pattern pattern))