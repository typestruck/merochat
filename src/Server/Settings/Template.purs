module Server.Settings.Template where

import Prelude
import Shared.Settings.Types

import Data.Date as DD
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now as EN
import Flame (QuerySelector(..))
import Flame as F
import Shared.Settings.View as SSV
import Shared.Types (PrimaryKey(..))

template :: Effect String
template  =
        F.preMount (QuerySelector ".settings-edition") {
                view: SSV.view,
                init: SettingsModel {
                        email: "",
                        emailConfirmation: "",
                        password: "",
                        passwordConfirmation: ""
                }
        }

