module Configuration where

import Prelude(bind, const, ($), pure)
import Node.FS.Sync(readTextFile)
import Node.Encoding(Encoding(..))
import Effect.Aff(launchAff)
import Simple.JSON(readJSON)
import Effect(Effect(..))
import Data.Either(either)
import Effect.Exception(throwException, error)

type Configuration = { port :: Int, development :: Boolean }

readConfiguration :: Effect Configuration
readConfiguration = do
	contents <- readTextFile UTF8 "../../configuration.json"
	either pure (const $ throwException $ error "Could not parse configuration") $ readJSON contents