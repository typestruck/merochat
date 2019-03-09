module Shared.Routing where

import Routing.Parser as P
import Prelude
import Shared.Types
import Data.Maybe(Maybe(..))
import Routing.Duplex.Generic as G
import Routing.Duplex as D
import Routing.Duplex (RouteDuplex')
import Routing.Duplex ((/))

routes :: RouteDuplex' Route
routes = D.root $ G.sum {
	"Landing" : G.noArgs,
	"Register" : "register" / G.noArgs,
	"Login": "login" ? { next : optional }
}

toResource :: Route -> String
toResource = D.print routes