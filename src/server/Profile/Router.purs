module Server.Profile.Router where

import Prelude

import Data.Either (Either(..))
import Data.String.Read as DSR
import HTTPure (Method(..), Request, (!@))
import HTTPure as H
import Run as R
import Run.Reader as RR
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Template as SPT
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Types (ResponseEffect)
import Shared.Router as SR
import Shared.Types

import Shared.Unsafe as SU

profile :: Request -> ResponseEffect
profile request@{ method, body } = do
        userID <- SRS.checkLogin request
        if method == Get then do
                profileUser <- SPD.presentProfile userID
                countries <- SPD.presentCountries
                languages <- SPD.presentLanguages
                contents <- R.liftEffect $ SPT.template {
                        user: profileUser,
                        countries,
                        languages
                }
                SRR.json' $ ProfileSettingsPayload contents
         else do
                SRR.json body (SPA.saveProfile userID)

generate :: Request -> ResponseEffect
generate request = do
        void $ SRS.checkLogin request
        case SR.toRoute $ H.fullPath request of
                Right (Generate { what }) -> do
                        generated <- SPA.generate what
                        SRR.json' generated
                _ -> SRR.throwBadRequest "invalid parameters"