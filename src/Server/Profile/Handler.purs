module Server.Profile.Handler where

import Prelude
import Server.Types
import Shared.Profile.Types

import Data.Maybe (Maybe)
import Run as R
import Server.Ok (Ok, ok)
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Shared.User
import Server.Profile.Template as SPT
import Shared.DateTime (DateWrapper(..))

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

generated ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { field ∷ What, value ∷ Maybe String } } → ServerEffect String
generated { guards: { loggedUserId }, body: { field, value } } = SPA.saveGeneratedField loggedUserId field value

avatar ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { base64 ∷ Maybe String } } → ServerEffect Ok
avatar { guards: { loggedUserId }, body: { base64 } } = do
      SPA.saveAvatar loggedUserId base64
      pure ok

age ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { birthday ∷ Maybe DateWrapper } } → ServerEffect Ok
age { guards: { loggedUserId }, body: { birthday } } = do
      SPA.saveAge loggedUserId birthday
      pure ok

gender ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { gender ∷ Maybe Gender } } → ServerEffect Ok
gender { guards: { loggedUserId }, body: { gender: picked } } = do
      SPA.saveGender loggedUserId picked
      pure ok

country ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { country ∷ Maybe Int } } → ServerEffect Ok
country { guards: { loggedUserId }, body: { country: id } } = do
      SPA.saveCountry loggedUserId id
      pure ok
