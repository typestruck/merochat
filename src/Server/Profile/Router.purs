module Server.Router.Profile where

import Prelude

import HTTPure (Request)
import Run as R
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Types (ResponseEffect)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Unsafe as SU
import Server.Profile.Database as SPD
import Shared.Types (JSONString(..), PrimaryKey(..))
import Run.Reader as RR

profile :: ResponseEffect
profile = do
        { session: { userID: maybeUserID } } <- RR.ask
        let userID = PrimaryKey $ SU.unsafeFromJust "router" maybeUserID
        profileUser <- SPD.presentUser userID
        contents <- R.liftEffect $ SPT.template profileUser
        SRR.json' $ JSONString contents
