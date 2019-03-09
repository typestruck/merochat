module Server.Routing where

import Effect.Class(liftEffect)
import HTTPure.Lookup((!@))
import Server.Response as R
import HTTPure as H
import HTTPure(ResponseM, Request)
import Server.Configuration(Configuration(..))
import Shared.Types
import Prelude
import Server.Template.Landing as L

router :: Configuration -> Request -> ResponseM
router configuration { path : [] } = do
	html <- liftEffect L.landing
      	R.html html
router configuration { path }
        | configuration.development && path !@ 0 == "client" = R.serveDevelopmentFile (path !@ 1) (path !@ 2)
        | otherwise = H.notFound