module Server.Routing (runRouter) where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Argonaut.Parser as P
import Data.Array as A
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Effect.Class.Console (log)
import HTTPure (Method(..), Request, ResponseM)
import Data.Maybe(Maybe(..))
import HTTPure as H
import HTTPure.Lookup ((!@))
import Partial.Unsafe (unsafePartial)
import Run as RU
import Run.Except as RE
import Run.Reader as RR
import Run.State as RS
import Server.Landing.Action as LA
import Server.Landing.Template as L
import Server.Response as R
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
		json body (LA.register "")
	--TODO: type this route
        | otherwise = do
		{ configuration : Configuration configuration } <- RR.ask

		if configuration.development && path !@ 0 == "client" then
			R.serveDevelopmentFile (path !@ 1) (path !@ 2)
		 else RU.liftAff H.notFound

json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< {message : _ }) runHandler $ P.jsonParser body
	where   runHandler arg = do
			response <- handler $ unsafePartial (DET.fromRight $ DGR.genericDecodeJson arg)
			R.json response

--needs as well as logging
runRouter :: ServerReader -> Request -> ResponseM
runRouter reading =
	RU.runBaseAff' <<<
        RE.catch R.requestError <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader reading <<<
	router