module Shared.Router (fromRoute, fromRouteToPath , toRoute) where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Int53 as DI
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Data.String.Read as DSR
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteError)
import Shared.Types
import Shared.Unsafe as SU

routes :: RouteDuplex' Route
routes = RD.root $ RDG.sum {
      "Landing" : RDG.noArgs,
      "Register" : "register" / RDG.noArgs,
      "Login": "login" ? { next: RD.optional <<< RD.string },
      "IM": "im" / RDG.noArgs,
      "SingleContact": "im" / "contact" ? { id: parsePrimaryKey },
      "Contacts" : "im" / "contacts" ? { skip: RD.int },
      "Suggestions" : "im" / "suggestions" / RDG.noArgs,
      "Profile": "profile" / RDG.noArgs,
      "Generate":  "profile" / "generate" ? { what: parseWhat },
      "Settings": "settings" / RDG.noArgs,
      "Reset": "reset" / RDG.noArgs,
      "AccountEmail": "settings" / "email" / RDG.noArgs,
      "Recover": "recover" ? { token: RD.optional <<< RD.string },
      "AccountPassword": "settings" / "password" / RDG.noArgs,
      "Terminate": "settings" / "close" / RDG.noArgs,
      "History" : "im" / "history" ? { skip: RD.int, with: parsePrimaryKey },
      "Block" : "im" / "block" ? { id: parsePrimaryKey }
}
      where parseWhat = RD.as show (DM.maybe (Left "error parsing what parameter") Right <<< DSR.read)
            parsePrimaryKey = RD.as (DS.replace (Pattern ".0") (Replacement "") <<< show <<< DI.toNumber <<< DN.unwrap) (DM.maybe (Left "error parsing what parameter") (Right <<< PrimaryKey) <<< DI.fromString)

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route
fromRoute :: Route -> String
fromRoute = RD.print routes

-- | Print a route without query string
fromRouteToPath :: Route -> String
fromRouteToPath = SU.fromJust <<< DA.head <<< DS.split (Pattern "?") <<< RD.print routes