module Server.Profile.Template where

import Prelude
import Shared.Profile.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Profile.View as SPV

template :: ProfileUser -> Effect String
template user = do
        F.preMount (QuerySelector ".profile-info-edition") {
                view: SPV.view,
                init: ProfileModel { user }
        }

