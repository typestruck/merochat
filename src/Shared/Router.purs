module Shared.Router (fromRoute, fromRouteAbsolute, toRoute) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe as DM
import Data.String as DS
import Data.String.Read as DSR
import Data.Symbol (SProxy(..))
import Routing.Duplex (RouteDuplex', (:=))
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
        "Profile": "profile" / RDG.noArgs,
        "Generate":  "profile" / "generate" ? { what: parseWhat }
}
        where parseWhat = RD.as show (DM.maybe (Left "error parsing what") Right <<< DSR.read)

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route including the initial /
fromRouteAbsolute :: Route -> String
fromRouteAbsolute = RD.print routes

-- | Print a route withouth /
fromRoute :: Route -> String
fromRoute = DM.maybe "" (_.tail) <<< DS.uncons <<< RD.print routes