module Server.Routing (router) where

import Prelude
import Server.Types

import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Argonaut.Parser as P
import Data.Array as A
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Effect.Class.Console (log)
import Shared.Types
import HTTPure (Method(..), Request, ResponseM)
import HTTPure as H
import HTTPure.Lookup ((!@))
import Partial.Unsafe (unsafePartial)
import Run.Reader as RR
import Server.Landing.Action as LA
import Server.Landing.Template as L
import Server.Response as R
import Run as RU
import Shared.Routing as SR

--TODO: logging

--split this into individual folders?
router :: Request -> ResponseEffect
router { headers, path, method, body }
	| A.null path = do
		html <- RU.liftEffect L.template
      		R.html html
	| method == Post && path == [ SR.fromRoute Register ] = do
		RU.liftEffect (log (show headers))
		requestJSON body (LA.register "")
	--TODO: type this route
        | otherwise = {- do
		{ configuration : Configuration configuration } <- RR.ask

		if configuration.development && path !@ 0 == "client" then
			R.serveDevelopmentFile (path !@ 1) (path !@ 2)
		 else -} RU.liftAff H.notFound

requestJSON ::  forall a b. Generic a b => DecodeRep b => String -> (a -> ResponseEffect) -> ResponseEffect
requestJSON body handler = DET.either R.badRequest (handler <<< unsafePartial (DET.fromRight <<< DGR.genericDecodeJson)) $ P.jsonParser body