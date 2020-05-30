module Server.Profile.Router where

import Prelude

import HTTPure (Method(..), Request)
import Run as R
import Run.Reader as RR
import Server.Profile.Database as SPD
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Types (ResponseEffect)
import Shared.Types (JSONResponse(..), PrimaryKey(..), Generate)
import Server.Profile.Action as SPA
import Shared.Unsafe as SU

profile :: Request -> ResponseEffect
profile { method, path, body } = SRS.ifLogged path do
        { session: { userID: maybeUserID } } <- RR.ask
        let userID = PrimaryKey $ SU.unsafeFromJust "router" maybeUserID
        if method == Get then do
                profileUser <- SPD.presentProfile userID
                countries <- SPD.presentCountries
                languages <- SPD.presentLanguages
                contents <- R.liftEffect $ SPT.template {
                        user: profileUser,
                        countries,
                        languages
                }
                SRR.json' $ JSONResponse contents
         else do
                SRR.json body (SPA.saveProfile userID)

generate :: Generate -> ResponseEffect
generate what = do
        generated <- SPA.generate what
        SRR.json' generated