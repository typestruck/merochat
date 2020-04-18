module Server.Router.Profile where

import Prelude

import HTTPure (Request)
import Run as R
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Types (ResponseEffect)
import Shared.Profile.Types (ProfileUser(..))
import Shared.Types (JSONString(..))

profile :: ResponseEffect
profile = do
        profileUser <- pure $ ProfileUser {}
        contents <- R.liftEffect $ SPT.template profileUser
        SRR.json' $ JSONString contents
