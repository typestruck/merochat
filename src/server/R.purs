module Server.R where

import Prelude
import Run as R
import Effect (Effect)
import Server.Types

serveTemplate :: Effect String -> ServerEffect Html
serveTemplate template = do
      contents <- R.liftEffect template
      pure $ Html contents
