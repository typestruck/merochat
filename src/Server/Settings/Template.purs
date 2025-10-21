module Server.Settings.Template where

import Effect (Effect)
import Flame as F
import Record as R
import Shared.Settings.Types (Tab(..), UserSettings)
import Shared.Settings.View as SSV
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ UserSettings → Effect String
template settings =
      F.preMount (QuerySelector "#settings-edition")
            { view: SSV.view
            , model:
                    R.merge
                          { email: ""
                          , emailConfirmation: ""
                          , password: ""
                          , passwordConfirmation: ""
                          , confirmTermination: false
                          , visible: true
                          , tab : Privacy
                          , erroredFields: []
                          , hideSuccessMessage: true
                          }
                          settings
            }
