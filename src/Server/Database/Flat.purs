module Server.Database.Flat where

import Data.Maybe as DM
import Data.String as DS
import Data.String (Pattern(..))
import Prelude
import Shared.Options.File (imageBasePath)

parseAvatar avatar = (imageBasePath <> _) <<< ("upload/" <> _ ) <$> avatar

splitAgg pattern = DM.maybe [] (DS.split (Pattern pattern))