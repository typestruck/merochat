module Shared.Router (fromRoute, fromRouteToPath , toRoute) where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read as DSR
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteError)
import Shared.Types (Route)
import Shared.Unsafe as SU

routes :: RouteDuplex' Route
routes = RD.root $ RDG.sum {
        "Landing" : RDG.noArgs,
        "Register" : "register" / RDG.noArgs,
        "Login": "login" ? { next: RD.optional <<< RD.string },
        "IM": "im" / RDG.noArgs,
        "Contacts" : "im" / "contacts" ? { page: RD.int },
        "Suggestions" : "im" / "suggestions" / RDG.noArgs,
        "Profile": "profile" / RDG.noArgs,
        "Generate":  "profile" / "generate" ? { what: parseWhat },
        "Settings": "settings" / RDG.noArgs,
        "AccountEmail": "settings" / "email" / RDG.noArgs,
        "AccountPassword": "settings" / "password" / RDG.noArgs,
        "Terminate": "settings" / "close" / RDG.noArgs
}
        where parseWhat = RD.as show (DM.maybe (Left "error parsing what parameter") Right <<< DSR.read)

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route
fromRoute :: Route -> String
fromRoute = RD.print routes

-- | Print a route without query string
fromRouteToPath :: Route -> String
fromRouteToPath = SU.unsafeFromJust "fromRouteToPath" <<< DA.head <<< DS.split (Pattern "?") <<< RD.print routes