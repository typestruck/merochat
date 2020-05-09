module Server.Profile.Template where

import Prelude
import Shared.Profile.Types

import Data.Date as DD
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now as EN
import Flame (QuerySelector(..))
import Flame as F
import Shared.DateTime as SDT
import Shared.Profile.View as SPV
import Shared.Types (PrimaryKey(..))

template :: { user :: ProfileUser, countries :: Array (Tuple PrimaryKey String), languages :: Array (Tuple PrimaryKey String) } -> Effect String
template {user: user@(ProfileUser { birthday }), countries, languages } = do
        minimumYear <- SDT.getMinimumYear
        F.preMount (QuerySelector ".profile-info-edition") {
                view: SPV.view minimumYear,
                init: ProfileModel {
                        isCountryVisible: true,
                        isGenderVisible: true,
                        isAgeVisible: true,
                        isDescriptionVisible: true,
                        isLanguagesVisible: true,
                        isTagsVisible: true,
                        birthday: Tuple (SDT.getYear <$> birthday) (Tuple (SDT.getMonth <$> birthday) (SDT.getDay <$> birthday)),
                        user,
                        countries,
                        languages
                }
        }

