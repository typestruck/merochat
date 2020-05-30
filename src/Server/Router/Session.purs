module Server.Router.Session where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import HTTPure (Headers, Response, ResponseM, Path)
import HTTPure.Lookup ((!@))
import Run.Reader as RR
import Server.Response as SRR
import Shared.Router as SRO
import Shared.Unsafe as SU

ifAnonymous :: ResponseEffect -> ResponseEffect
ifAnonymous handler = do
        { session : { userID } } <- RR.ask
        if DM.isNothing userID then
                handler
         else
                SRR.redirect $ SRO.fromRoute IM

ifLogged :: Path -> ResponseEffect -> ResponseEffect
ifLogged path handler = do
        { session : { userID } } <- RR.ask
        if DM.isJust userID then
                handler
         else
                SRR.redirect <<< SRO.fromRoute $ Login { next: Just (path !@ 0) }

loggedUserID :: ServerEffect PrimaryKey
loggedUserID = do
        { session: { userID: maybeUserID } } <- RR.ask
        pure <<< PrimaryKey $ SU.unsafeFromJust "loggedUserId" maybeUserID
