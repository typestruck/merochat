module Server.Router.Profile where

import Prelude

import HTTPure (Method(..), Request)
import Run as R
import Run.Reader as RR
import Server.Profile.Database as SPD
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Types (ResponseEffect)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (JSONString(..), PrimaryKey(..))
import Server.Profile.Action as SPA
import Shared.Unsafe as SU

profile :: Request -> ResponseEffect
profile { method, path, body } = SRS.ifLogged path do
        if method == Get then do
                { session: { userID: maybeUserID } } <- RR.ask
                let userID = PrimaryKey $ SU.unsafeFromJust "router" maybeUserID
                profileUser <- SPD.presentUser userID
                contents <- R.liftEffect $ SPT.template profileUser
                SRR.json' $ JSONString contents
         else do
                SRR.json body SPA.saveProfile

