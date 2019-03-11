module Server.Routing where

import Prelude
import Shared.Types

import Data.Array as A
import Effect.Class (liftEffect)
import HTTPure (Method(..), Request, ResponseM)
import HTTPure as H
import HTTPure.Lookup ((!@))
import Server.Configuration (Configuration(..))
import Server.RegisterLogin as RL
import Server.Response as R
import Server.Routing as SR
import Server.Template.Landing as L

--split this into individual folders?
router :: Configuration -> Request -> ResponseM
router configuration { path, method }
	| A.null path = do
		html <- liftEffect L.landing
      		R.html html
	| method == Post && path == [ SR.toResource Register ] = RL.register
	--TODO: type this route
        | configuration.development && path !@ 0 == "client" = R.serveDevelopmentFile (path !@ 1) (path !@ 2)
        | otherwise = H.notFound