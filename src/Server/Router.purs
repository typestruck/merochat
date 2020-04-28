-- | The entry point for routing. Controller logic is in the particular Router files.
module Server.Router (
        runRouter,
        session
) where

import Prelude
import Server.Types
import Shared.Types

import Browser.Cookies.Data (Cookie(..))
import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import HTTPure (Method(..), Request, ResponseM)
import HTTPure.Lookup ((!@))
import Run as R
import Run.Except as RE
import Run.Reader as RR

import Server.Landing.Router as SLR
import Server.Login.Router as SLIR
import Server.IM.Router as SIR
import Server.Response as SRR
import Server.Token as ST
import Shared.Cookies (cookieName)
import Shared.Header (xAccessToken)
import Shared.Router as SRO
import Server.Router.Profile as SRP

--needs logging as well
runRouter :: ServerReader -> Request -> ResponseM
runRouter reading =
        R.runBaseAff' <<<
        RE.catch SRR.requestError <<<
        RR.runReader reading <<<
        router

--move path matching to individual folders?
router :: Request -> ResponseEffect
router request@{ headers, path, method }
        --landing
        | DA.null path = SLR.landing
        | path == [SRO.fromRoute Register] && method == Post = SLR.register request
        --login
        | path !@ 0 == (SRO.fromRoute $ Login { next: Nothing }) = SLIR.login request
        --im
        | path == [SRO.fromRoute IM] = SIR.im request
        --profile
        | path == [SRO.fromRoute Profile] = SRP.profile request
        --local files and 404 for development
        | otherwise = do
                { configuration : Configuration configuration } <- RR.ask

                if configuration.development && path !@ 0 == "client" then
                        SRR.serveDevelopmentFile (path !@ 1) (path !@ 2)
                 else if configuration.development && path !@ 0 == "favicon.ico" then
                        SRR.serveDevelopmentFile "media" "favicon.ico"
                 else
                        RE.throw $ NotFound { reason: "Could not find resource: " <> show path, isPost: method == Post}

-- | Extracts an user id from a json web token. GET requests should have it in cookies, otherwise in the x-access-token header
session :: Configuration -> Request -> Effect Session
session (Configuration configuration) { headers, method } = do
        map { userID: _ } $ if method == Get then
                                sessionFromCookie $ BCI.bakeCookies (headers !@ "Cookie")
                             else
                                sessionFromXHeader (headers !@ xAccessToken)
        where   sessionFromCookie cookies =
                        case DA.find (\(Cookie {key}) -> cookieName == key) cookies of
                                Just (Cookie {value}) -> ST.userIDFromToken configuration.tokenSecretGET value
                                _ -> pure Nothing

                sessionFromXHeader value = ST.userIDFromToken configuration.tokenSecretPOST value

