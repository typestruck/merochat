module Server.Router.Session where

import Server.Types
import Data.Maybe as DM
import Shared.Router as SRO
import HTTPure (Headers, Response, ResponseM, Path)
import Server.Response as SRR
import Prelude
import Run.Reader as RR
import Data.Maybe(Maybe(..))
import HTTPure.Lookup ((!@))
import Shared.Types

ifAnonymous :: ResponseEffect -> ResponseEffect
ifAnonymous handler = do
        { session : { userID } } <- RR.ask
        if DM.isNothing userID then
                handler
         else
                SRR.redirect $ SRO.fromRouteAbsolute IM

ifLogged :: Path -> ResponseEffect -> ResponseEffect
ifLogged path handler = do
        { session : { userID } } <- RR.ask
        if DM.isJust userID then
                handler
         else
                SRR.redirect <<< SRO.fromRouteAbsolute $ Login { next: Just (path !@ 0) }