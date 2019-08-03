module Server.Routing (
	runRouter,
	session
) where

import Prelude
import Server.Types
import Shared.Types

import Browser.Cookies.Data (Cookie(..))
import Browser.Cookies.Internal as BCI
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Parser as DAP
import Data.Array as DA
import Effect (Effect)
import Data.Array as DS
import Data.Either as DET
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import HTTPure (Method(..), Request, ResponseM)
import HTTPure.Lookup ((!@))
import Partial.Unsafe (unsafePartial)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Run.State as RS
import Server.Landing.Action as SLA
import Server.Login.Action as SLI
import Server.Landing.Template as SLT
import Server.Login.Template as SLIT
import Server.Response as SRR
import Shared.Routing as SRO
import Server.Token as ST
import Shared.Header (xAccessToken)

--TODO: logging

--split this into individual folders?
router :: Request -> ResponseEffect
router { headers, path, method, body }
	| DA.null path = serveTemplate SLT.template
	| path !@ 0 == (SRO.fromRoute $ Login { next: Nothing }) =
		if method == Get then
			serveTemplate SLIT.template
		 else
		 	json body SLI.login
	| path == [ SRO.fromRoute Register ] && method == Post = json body (SLA.register "")
	--TODO: type this route
        | otherwise = do
		{ configuration : Configuration configuration } <- RR.ask

		if configuration.development && path !@ 0 == "client" then
			SRR.serveDevelopmentFile (path !@ 1) (path !@ 2)
		 else
			RE.throw $ NotFound { reason: "Could not find resource: " <> show path, isPost: method == Post}

-- ifAnonymous ::

-- ifLogged ::

serveTemplate :: Effect String -> ResponseEffect
serveTemplate template = do
	html <- R.liftEffect template
      	SRR.html html

json :: forall a b c d. Generic a b => EncodeRep b => Generic c d => DecodeRep d => String -> (c -> ServerEffect a) -> ResponseEffect
json body handler = DET.either (RE.throw <<< InternalError <<< { reason : _ }) runHandler $ DAP.jsonParser body
	where   runHandler arg = do
			response <- handler $ unsafePartial (DET.fromRight $ DADGR.genericDecodeJson arg)
			SRR.json response

-- | Extracts an user id from a json web token. GET requests should have it in cookies, otherwise in the x-access-token header
session :: Configuration -> Request -> Effect Session
session (Configuration configuration) { headers, method } = do
	map { userID: _ } $ if method == Get then
				sessionFromCookie $ BCI.bakeCookies (headers !@ "Cookie")
			     else
				sessionFromXHeader (headers !@ xAccessToken)
	where 	sessionFromCookie =
			case _ of -- we might have other cookies later...
				[Cookie { value }] -> ST.userIDFromToken configuration.tokenSecretGET value
				_ -> pure Nothing

		sessionFromXHeader value = ST.userIDFromToken configuration.tokenSecretPOST value

--needs logging as well
runRouter :: ServerReader -> ServerState -> Request -> ResponseM
runRouter reading stating =
	R.runBaseAff' <<<
        RE.catch SRR.requestError <<<
        RS.evalState stating <<<
        RR.runReader reading <<<
	router