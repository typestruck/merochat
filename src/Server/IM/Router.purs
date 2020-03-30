module Server.IM.Router where

import Prelude
import Server.IM.Action as SIA
import Shared.Types
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Types
import Server.Router.Session as SRS
import Run.Reader as RR
import Server.Response as SRR
import Shared.Unsafe as SU
import HTTPure (Method(..), Request, ResponseM, Path)

im :: Request -> ResponseEffect
im { path } = do
        let handler = do
                { session: { userID: maybeUserID } } <- RR.ask
                let userID = PrimaryKey $ SU.unsafeFromJust "router" maybeUserID
                user <- SID.presentUser userID
                suggestions <- SIA.suggest userID
                contacts <- SIA.contactList userID
                SRR.serveTemplate $ SIT.template { contacts, suggestions, user }
        SRS.ifLogged path handler