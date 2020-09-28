module Server.Profile.Template where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.DateTime as SDT
import Shared.Profile.View as SPV

template :: { user :: ProfileUser, countries :: Array (Tuple PrimaryKey String), languages :: Array (Tuple PrimaryKey String) } -> Effect String
template {user: user, countries, languages } = do
      F.preMount (QuerySelector ".profile-edition") {
            view: SPV.view,
            init: {
                  countryInputed: Nothing,
                  genderInputed: Nothing,
                  ageInputed: Nothing,
                  nameInputed: Nothing,
                  headlineInputed: Nothing,
                  descriptionInputed: Nothing,
                  isEditingLanguages: false,
                  isEditingTags: false,
                  user,
                  countries,
                  languages
            }
      }

