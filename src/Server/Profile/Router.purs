module Server.Profile.Router where

import Prelude

import Data.String.Read as DSR
import HTTPure (Method(..), Request, (!@))
import Run as R
import Run.Reader as RR
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Types (ResponseEffect)
import Shared.Types (JSONResponse(..), PrimaryKey(..))
import Shared.Unsafe as SU

profile :: Request -> ResponseEffect
profile { method, path, body } = SRS.ifLogged path do
        userID <- SRS.loggedUserID
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

generate :: Request -> ResponseEffect
generate { query } = do
        --REFACTOR: the query parameters outside of the Shared.Router functions are not type safe
        generated <- SPA.generate <<< SU.fromJust "generate" $ DSR.read (query !@ "what")
        SRR.json' generated