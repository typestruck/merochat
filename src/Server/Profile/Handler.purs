module Server.Profile.Handler where

import Prelude
import Server.Effect
import Shared.Profile.Types
import Shared.User

import Data.Maybe (Maybe(..))
import Run as R
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Template as SPT
import Shared.Post (Post)

profile ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
profile { guards: { loggedUserId } } = do
      profileUser ← SPDF.fromFlatProfileUser <$> SPD.presentProfile loggedUserId
      countries ← SPD.presentCountries
      languages ← SPD.presentLanguages
      posts ← SPD.presentPosts loggedUserId Nothing
      R.liftEffect $ SPT.template
            { user: profileUser
            , posts
            , countries
            , languages
            }

generated ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { field ∷ What } } → ServerEffect String
generated { body } = SPA.generateField body.field

posts ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { after ∷ Maybe Int } } → ServerEffect (Array Post)
posts request = SPA.refreshPosts request.guards.loggedUserId request.query.after

save ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ SavedFields } → ServerEffect { avatar ∷ Maybe String }
save request = SPA.save request.guards.loggedUserId request.body