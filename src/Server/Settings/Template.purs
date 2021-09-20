module Server.Settings.Template where

import Shared.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Settings.View as SSV
import Shared.User (ProfileVisibility)

template ∷ ProfileVisibility -> Effect String
template profileVisibility =
      F.preMount (QuerySelector ".settings-edition")
            { view: SSV.view
            , init:
                    { email: ""
                    , emailConfirmation: ""
                    , password: ""
                    , passwordConfirmation: ""
                    , confirmTermination: false
                    , profileVisibility
                    , erroredFields: []
                    }
            }
