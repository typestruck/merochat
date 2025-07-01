module Server.Profile.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Server.Profile.Types (Payload)
import Shared.Element (ElementId(..))
import Shared.Profile.View as SPV

template ∷ Payload → Effect String
template { user: user, countries, languages } = do
      F.preMount (QuerySelector ("#" <> show ProfileEditionForm))
            { view: SPV.view
            , init:
                    { nameInputed: Nothing
                    , headlineInputed: Nothing
                    , ageInputed: Nothing
                    , genderInputed: Nothing
                    , countryInputed: Nothing
                    , languagesInputed: Nothing
                    , visible: true
                    , languagesInputedList: Nothing
                    , tagsInputed: Nothing
                    , tagsInputedList: Nothing
                    , descriptionInputed: Nothing
                    , loading: false
                    , registrationMessage: false
                    , updateRequestStatus: Nothing
                    , user
                    , countries
                    , languages
                    }
            }
