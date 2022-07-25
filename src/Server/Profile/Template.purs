module Server.Profile.Template where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Server.Profile.Types (Payload)
import Shared.Profile.View as SPV

template ∷ Payload → Effect String
template { user: user, countries, languages } = do
      F.preMount (QuerySelector "#profile-edition-form")
            { view: SPV.view
            , init:
                    { nameInputed: Nothing
                    , headlineInputed: Nothing
                    , ageInputed: Nothing
                    , genderInputed: Nothing
                    , countryInputed: Nothing
                    , languagesInputed: Nothing
                    , languagesInputedList: Nothing
                    , tagsInputed: Nothing
                    , tagsInputedList: Nothing
                    , descriptionInputed: Nothing
                    , generating: Nothing
                    , experimenting: Nothing
                    , hideSuccessMessage: false
                    , user
                    , countries
                    , languages
                    }
            }
