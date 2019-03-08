module Routing where

import Effect.Class(liftEffect)
import Template.Landing
import HTTPure.Lookup((!@))
import Response as R
import HTTPure as H
import Shared.Types


router configuration { path : [] } = do
	html <- liftEffect L.landing
      	R.html html
router configuration { path }
        | configuration.development && path !@ 0 == "client" = R.serveDevelopmentFile (path !@ 1) (path !@ 2)
        | otherwise = H.notFound