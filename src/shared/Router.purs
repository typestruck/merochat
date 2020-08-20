module Shared.Router (fromRoute, fromRouteToPath , toRoute) where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Either (Either(..))
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as DFD
import Data.Int53 as DI
import Data.List as DL
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
import Shared.DateTime (dateFormat)
import Shared.DateTime as SDT
import Shared.Unsafe as SU

--REFACTOR: use payload type level urls
routes :: RouteDuplex' Route
routes = RD.root $ RDG.sum {
      "Landing" : RDG.noArgs,
      "Login": "login" ? { next: RD.optional <<< RD.string },
      "IM": "im" / RDG.noArgs,
      "Suggestions" : "im" / "suggestions" / RDG.noArgs,
      "Profile": "profile" / RDG.noArgs,
      "Generate":  "profile" / "generate" ? { what: parseWhat },
      "Settings": "settings" / RDG.noArgs,
      "Reset": "reset" / RDG.noArgs,
      "AccountEmail": "settings" / "email" / RDG.noArgs,
      "Recover": "recover" ? { token: RD.optional <<< RD.string },
      "AccountPassword": "settings" / "password" / RDG.noArgs,
      "Terminate": "settings" / "close" / RDG.noArgs,
      "Block" : "im" / "block" ? { id: parsePrimaryKey },
      "MissedMessages": "im" / "missed" ? { since: parseDate }
}
      where parseWhat = RD.as show (DM.maybe (Left "error parsing what parameter") Right <<< DSR.read)
            parsePrimaryKey = RD.as (DI.toString <<< DN.unwrap) (DM.maybe (Left "error parsing what parameter") (Right <<< PrimaryKey) <<< DI.fromString)
            parseDate = RD.as SDT.formatDateTime SDT.unformatDateTime

toRoute :: String -> Either RouteError Route
toRoute = RD.parse routes

-- | Print a route
fromRoute :: Route -> String
fromRoute = RD.print routes

-- | Print a route without query string
fromRouteToPath :: Route -> String
fromRouteToPath = SU.fromJust <<< DA.head <<< DS.split (Pattern "?") <<< RD.print routes