module Shared.Router (fromRoute, fromRoute, toRoute) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe as DM
import Data.String as DS
import Data.String.Read as DSR
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteError)
import Shared.Types (Route)

routes :: RouteDuplex' Route
routes = RD.root $ RDG.sum {
        "Landing" : RDG.noArgs,
        "Register" : "register" / RDG.noArgs,
        "Login": "login" ? { next: RD.optional <<< RD.string },
        "IM": "im" / RDG.noArgs,
        "Contacts" : "im" / "contacts" ? { page: RD.int },
        "Profile": "profile" / RDG.noArgs,
        "Generate":  "profile" / "generate" ? { what: parseWhat },
        "Settings": "settings" / RDG.noArgs,
        "AccountEmail": "settings" / "email" / RDG.noArgs,
        "AccountPassword": "settings" / "password" / RDG.noArgs,
        "Terminate": "settings" / "close" / RDG.noArgs
}
        where parseWhat = RD.as show (DM.maybe (Left "error parsing what") Right <<< DSR.read)

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route
fromRoute :: Route -> String
fromRoute = RD.print routes

-- | Print a route without query string
fromRouteToPath :: Route -> String
fromRouteToPath = <<< RD.print routes