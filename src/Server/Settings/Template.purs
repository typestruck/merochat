module Server.Settings.Template where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Record as R
import Shared.Settings.Types (PrivacySettings, Tab(..))
import Shared.Settings.View as SSV
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ PrivacySettings → Effect String
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
                          , chatBackground: Nothing
                          , tab : Privacy
                          , erroredFields: []
                          , hideSuccessMessage: true
                          }
                          settings
            }
