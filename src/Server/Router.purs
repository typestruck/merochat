-- | The entry point for routing. Controller logic is in the particular Router files.
module Server.Router (
        runRouter,
        session
) where

import Browser.Cookies.Data (Cookie(..))
import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import HTTPure (Method(..), Request, ResponseM)
import HTTPure as H
import HTTPure.Lookup ((!@))
import Prelude (bind, map, otherwise, pure, show, ($), (&&), (<<<), (<>), (==))
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.IM.Router as SIR
import Server.Landing.Router as SLR
import Server.Login.Router as SLIR
import Server.Profile.Router as SPR
import Server.Response as SRR
import Server.Settings.Router as SSR
import Server.Token as ST
import Server.Types (Configuration(..), ResponseEffect, ServerReader, Session)
import Shared.Cookies (cookieName)
import Shared.Header (xAccessToken)
import Shared.Router as SRO
import Shared.Types (Generate(..), ResponseError(..), Route(..))

--needs logging as well
runRouter :: ServerReader -> Request -> ResponseM
runRouter reading =
        R.runBaseAff' <<<
        RE.catch SRR.requestError <<<
        RR.runReader reading <<<
        router

--move path matching to individual folders?
router :: Request -> ResponseEffect
router request@{ headers, path, method } =
        --landing
       if paths == SRO.fromRoute Landing then
              SLR.landing
        else if paths == SRO.fromRoute Register && method == Post then
              SLR.register request
        --login
        else if paths == SRO.fromRouteToPath (Login { next: Nothing }) then
              SLIR.login request
        --im
        else if paths == SRO.fromRoute IM then
              SIR.im request
        else if paths == SRO.fromRouteToPath (Contacts { page: 0 }) then
              SIR.contacts request
        else if paths == SRO.fromRoute Suggestions then
              SIR.suggestions
        --profile
        else if paths == SRO.fromRoute Profile then
              SPR.profile request
        else if paths == SRO.fromRouteToPath (Generate { what: Name }) then
              SPR.generate request
        --settings
        else if paths == SRO.fromRoute Settings then
              SSR.settings request
        else if paths == SRO.fromRoute AccountEmail && method == Post then
              SSR.changeEmail request
        else if paths == SRO.fromRoute AccountPassword && method == Post then
              SSR.changePassword request
        else if paths == SRO.fromRoute Terminate && method == Post then
              SSR.terminateAccount request
        --local files and 404 for development
        else do
              { configuration : Configuration configuration } <- RR.ask

              if configuration.development && path !@ 0 == "client" then
                     SRR.serveDevelopmentFile path
               else if configuration.development && path !@ 0 == "favicon.ico" then
                     SRR.serveDevelopmentFile ["media", "favicon.ico"]
               else
                     RE.throw $ NotFound { reason: "Could not find resource: " <> H.fullPath request, isPost: method == Post}
       where paths = "/" <> DS.joinWith "/" path

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

