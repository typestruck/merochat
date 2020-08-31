module Server.Profile.Template where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.DateTime as SDT
import Shared.Profile.View as SPV
import Shared.Types

template :: { user :: ProfileUser, countries :: Array (Tuple PrimaryKey String), languages :: Array (Tuple PrimaryKey String) } -> Effect String
template {user: user@{ birthday }, countries, languages } = do
      minimumYear <- SDT.getEarliestYear
      F.preMount (QuerySelector ".profile-info-edition") {
            view: SPV.view minimumYear,
            init: {
                  isCountryVisible: true,
                  isGenderVisible: true,
                  isAgeVisible: true,
                  isLanguagesVisible: true,
                  isTagsVisible: true,
                  birthday: Tuple (SDT.getYear <$> birthday) (Tuple (SDT.getMonth <$> birthday) (SDT.getDay <$> birthday)),
                  user,
                  countries,
                  languages
            }
      }

