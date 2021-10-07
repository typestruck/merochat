module Server.Profile.Handler where

import Prelude
import Server.Types
import Shared.ContentType

import Run as R
import Server.Ok
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Template as SPT
import Shared.Profile.Types (Generate, ProfileUser)

profile ∷ { guards ∷ { loggedUserID ∷ Int } } → ServerEffect String
profile { guards: { loggedUserID } } = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserID
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      R.liftEffect $ SPT.template
            { user: profileUser
            , countries
            , languages
            }

profileUpdate ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ ProfileUser } → ServerEffect Ok
profileUpdate { guards: { loggedUserID }, body } = SPA.saveProfile loggedUserID body

generate ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { what ∷ Generate } } → ServerEffect String
generate { query: { what } } = SPA.generate what