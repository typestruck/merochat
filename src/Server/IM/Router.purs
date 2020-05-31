module Server.IM.Router where

import Prelude
import Server.Types
import Shared.Types

import Data.Int as DI
import HTTPure (Request, (!@))
import Run.Reader as RR
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Response as SRR
import Server.Router.Session as SRS
import Shared.Unsafe as SU

im :: Request -> ResponseEffect
im { path } = do
        let handler = do
                userID <- SRS.loggedUserID
                user <- SID.presentUser userID
                suggestions <- SIA.suggest userID
                contacts <- SIA.contactList userID 0
                SRR.serveTemplate $ SIT.template { contacts, suggestions, user }
        SRS.ifLogged path handler

contacts :: Request -> ResponseEffect
contacts { query } = do
        userID <- SRS.loggedUserID
        list <- SIA.contactList userID <<< SU.unsafeFromJust "contacts" $ DI.fromString (query !@ "page")
        SRR.json' $ JSONResponse list