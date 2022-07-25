module Server.Profile.Handler where

import Prelude
import Server.Types

import Run as R
import Server.Ok (Ok, ok)
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Template as SPT
import Shared.Profile.Types (Generate, ProfileUser)

profile ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
profile { guards: { loggedUserId } } = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserId
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      R.liftEffect $ SPT.template
            { user: profileUser
            , countries
            , languages
            }

profileUpdate ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ ProfileUser } → ServerEffect Ok
profileUpdate { guards: { loggedUserId }, body } = do
      SPA.saveProfile loggedUserId body
      pure ok

generate ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { what ∷ Generate } } → ServerEffect String
generate { query: { what } } = SPA.generate what