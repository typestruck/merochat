module Shared.Routing (fromRoute, toRoute) where

import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Prelude ((#), ($))
import Routing.Duplex (RouteDuplex', (:=))
import Routing.Duplex as D
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)
import Shared.Types (Route)

routes :: RouteDuplex' Route
routes = D.root $ G.sum {
	"Landing" : G.noArgs,
	"Register" : "register" / G.noArgs,
	--the type level voodoo on the examples doesn't typecheck
	"Login": D.path "login" (D.record # _next := D.optional (D.param "next")),
	"IM": "im" / G.noArgs
}
	where     _next = SProxy :: SProxy "next"

toRoute :: String -> Either RouteError Route
toRoute = D.parse routes

fromRoute :: Route -> String
fromRoute = D.print routes