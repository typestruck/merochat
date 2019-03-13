module Server.Routing (router) where

import Prelude
import Shared.Types

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as D
import Data.Argonaut.Parser as P
import Data.Array as A
import Data.Either as DET
import Effect.Class (liftEffect)
import HTTPure (Method(..), Request, ResponseM)
import HTTPure as H
import HTTPure.Lookup ((!@))
import Server.Configuration (Configuration(..))
import Server.Landing.Action as LA
import Server.Landing.Template as L
import Server.RegisterLogin as RL
import Server.Response as R
import Server.Routing as SR

--TODO: logging

--split this into individual folders?
router :: Configuration -> Request -> ResponseM
router configuration { path, method, body }
	| A.null path = do
		html <- liftEffect L.template
      		R.html html
	| method == Post && path == [ SR.toRoute Register ] = requestJSON body LA.register
	--TODO: type this route
        | configuration.development && path !@ 0 == "client" = R.serveDevelopmentFile (path !@ 1) (path !@ 2)
        | otherwise = H.notFound

requestJSON :: forall a b. DecodeJson a => String -> (a -> ResponseM) -> ResponseM
requestJSON body handler = DET.either R.badRequest handler (D.decodeJson >>= P.jsonParser body)