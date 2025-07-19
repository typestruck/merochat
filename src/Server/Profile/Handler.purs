module Server.Profile.Handler where

import Prelude
import Server.Effect
import Shared.Profile.Types

import Data.Maybe (Maybe)
import Run as R
import Server.Ok (Ok, ok)
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Shared.User
import Server.Profile.Template as SPT
import Shared.DateTime (DateWrapper)

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

generated ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { field ∷ What} } → ServerEffect String
generated { body } = SPA.generateField body.field

save ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ SavedFields } → ServerEffect Ok
save request = do
      SPA.save request.guards.loggedUserId request.body
      pure ok