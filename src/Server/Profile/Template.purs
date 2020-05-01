module Server.Profile.Template where

import Prelude
import Shared.Profile.Types

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Profile.View as SPV

template :: ProfileUser -> Array (Tuple Int String) -> Effect String
template user countries = do
        F.preMount (QuerySelector ".profile-info-edition") {
                view: SPV.view,
                init: ProfileModel {
                        isCountryVisible: true,
                        isGenderVisible: true,
                        isAgeVisible: true,
                        isLanguagesVisible: true,
                        user,
                        countries }
        }

