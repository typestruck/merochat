module Server.Settings.Template where

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Record as R
import Shared.Settings.Types (PrivacySettings)
import Shared.Settings.View as SSV

template ∷ PrivacySettings → Effect String
template settings =
      F.preMount (QuerySelector "#settings-edition")
            { view: SSV.view
            , init:
                    R.merge
                          { email: ""
                          , emailConfirmation: ""
                          , password: ""
                          , passwordConfirmation: ""
                          , confirmTermination: false
                          , visible : true
                          , erroredFields: []
                          , hideSuccessMessage: true
                          }
                          settings
            }
