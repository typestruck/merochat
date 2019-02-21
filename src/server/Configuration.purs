module Configuration where

import Prelude(bind, const, ($), pure)
import Node.FS.Aff(readTextFile)
import Node.Encoding(Encoding(..))
import Effect.Aff(launchAff)
import Simple.JSON(readJSON)
import Effect(Effect(..))
import Data.Either(either)
import Effect.Exception(throwException, error)

type Configuration = { port :: Int, development :: Boolean }

readConfiguration :: Effect Configuration
readConfiguration = do
	contents <- launchAff $ readTextFile UTF8 "../../configuration.json"
	either pure (const $ throwException $ error "Could not parse configuration") $ readJSON contents