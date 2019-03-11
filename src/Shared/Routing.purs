module Shared.Routing (fromResource, toResource) where

import Prelude
import Shared.Types

import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Routing.Duplex (RouteDuplex', (:=))
import Routing.Duplex as D
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

routes :: RouteDuplex' Route
routes = D.root $ G.sum {
	"Landing" : G.noArgs,
	"Register" : "register" / G.noArgs,
	--the type level voodoo on the examples doesn't typecheck
	"Login": D.path "login" (D.record # _next := D.optional (D.param "next"))
}
	where     _next = SProxy :: SProxy "next"

fromResource :: String -> Either RouteError Route
fromResource = D.parse routes

toResource :: Route -> String
toResource = D.print routes