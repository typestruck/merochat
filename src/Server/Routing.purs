module Server.Routing (runRouter) where

import Prelude
import Server.Types
import Shared.Types

import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Parser as DAP
import Data.Array as DA
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Effect.Console as EC
import HTTPure (Method(..), Request, ResponseM)
import Data.Maybe(Maybe(..))
import HTTPure.Lookup ((!@))
import Partial.Unsafe (unsafePartial)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Run.State as RS
import Server.Landing.Action as SLA
import Server.Landing.Template as SLL
import Server.Response as SRR
import Shared.Routing as SRO

--TODO: logging

--split this into individual folders?
router :: Request -> ResponseEffect
router { headers, path, method, body }
	| DA.null path = do
		html <- R.liftEffect SLL.template
      		SRR.html html
	| method == Post && path == [ SRO.fromRoute Register ] = do
		json body (SLA.register "")
	--TODO: type this route
        | otherwise = do
		{ configuration : Configuration configuration } <- RR.ask

		if configuration.development && path !@ 0 == "client" then
			SRR.serveDevelopmentFile (path !@ 1) (path !@ 2)
		 else
			RE.throw $ NotFound { reason: "Could not find resource: " <> show path, isPost: method == Post}

json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< { reason : _ }) runHandler $ DAP.jsonParser body
	where   runHandler arg = do
			response <- handler $ unsafePartial (DET.fromRight $ DADGR.genericDecodeJson arg)
			SRR.json response

--needs logging as well
runRouter :: ServerReader -> Request -> ResponseM
runRouter reading =
	R.runBaseAff' <<<
        RE.catch SRR.requestError <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader reading <<<
	router