module Server.Settings.Template where

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Settings.View as SSV
import Shared.Types

template :: Effect String
template =
      F.preMount (QuerySelector ".settings-edition") {
            view: SSV.view,
            init: {
                  email: "",
                  emailConfirmation: "",
                  password: "",
                  passwordConfirmation: "",
                  confirmTermination: false,
                  erroredFields: []
            }
      }

