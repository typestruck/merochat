module Shared.Routing (fromRoute, fromRouteAbsolute, toRoute) where

import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Prelude
import Routing.Duplex (RouteDuplex', (:=))
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/))
import Data.Maybe as DM
import Data.String as DS
import Routing.Duplex.Parser (RouteError)
import Shared.Types (Route)

routes :: RouteDuplex' Route
routes = RD.root $ RDG.sum {
        "Landing" : RDG.noArgs,
        "Register" : "register" / RDG.noArgs,
        --the type level voodoo on the examples doesn't typecheck
        "Login": RD.path "login" (RD.record # _next := RD.optional (RD.param "next")),
        "IM": "im" / RDG.noArgs
}
        where     _next = SProxy :: SProxy "next"

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route including the initial /
fromRouteAbsolute :: Route -> String
fromRouteAbsolute = RD.print routes

-- | Print a route withouth /
fromRoute :: Route -> String
fromRoute = DM.maybe "" (_.tail) <<< DS.uncons <<< RD.print routes