module Server.Profile.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Server.Profile.Types (Payload)
import Shared.Element (ElementId(..))
import Shared.Profile.Types (ProfileMode(..))
import Shared.Profile.View as SPV

template ∷ Payload → Effect String
template { user: user, countries, languages } = do
      F.preMount (QuerySelector ("#" <> show ProfileEditionForm))
            { view: SPV.view
            , init:
                    { nameInputed: Just user.name
                    , headlineInputed: Just user.headline
                    , ageInputed:  user.age
                    , genderInputed: user.gender
                    , mode : Edit
                    , fromTemporary : false
                    , countryInputed: user.country
                    , languagesInputed: user.languages
                    , visible: true
                    , generated : []
                    , avatarInputed : user.avatar
                    , tagsInputed: user.tags
                    , descriptionInputed:  Just user.description
                    , loading: false
                    , registrationMessage: false
                    , updateRequestStatus: Nothing
                    , user
                    , countries
                    , languages
                    }
            }
