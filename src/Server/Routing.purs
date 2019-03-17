module Server.Routing (router) where

import Prelude
import Shared.Types

import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Argonaut.Parser as P
import Data.Array as A
import Data.Either as DET
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure (Method(..), Request, ResponseM)
import HTTPure as H
import HTTPure.Lookup ((!@))
import Server.Landing.Action as LA
import Server.Landing.Template as L
import Server.Response as R
import Shared.Routing as SR
import Server.Types
import Run.Reader as RR
import Partial.Unsafe(unsafePartial)
import Data.Generic.Rep (class Generic)

--TODO: logging

--split this into individual folders?
router :: Request -> ResponseEffect
router { headers, path, method, body }
	| A.null path = do
		html <- liftEffect L.template
      		R.html html
	| method == Post && path == [ SR.toRoute Register ] = do
		liftEffect (log (show headers))
		requestJSON body (LA.register "")
	--TODO: type this route
        | otherwise = do
		{ configuration : Configuration configuration } <- RR.ask

		if configuration.development && path !@ 0 == "client" then
			R.serveDevelopmentFile (path !@ 1) (path !@ 2)
		 else H.notFound

requestJSON ::  forall a b. Generic a b => DecodeRep b => String -> (a -> ResponseEffect) -> ResponseEffect
requestJSON body handler = DET.either R.badRequest (handler <<< unsafePartial (DET.fromRight <<< DGR.genericDecodeJson)) $ P.jsonParser body