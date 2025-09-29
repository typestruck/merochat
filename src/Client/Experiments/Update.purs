module Client.Experiments.Update where

import Prelude
import Shared.Experiments.Types

import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame (Update)
import Flame as F
import Flame.Subscription as FS
import Shared.Im.Types (ImMessage(..), RetryableRequest(..))
import Shared.Modal.Types (Modal(..), ScreenModal(..))
import Shared.Options.MountPoint (imId)

update ∷ Update ChatExperimentModel ChatExperimentMessage
update model =
      case _ of
            ToggleVisibility modal → model { visible = modal == ShowExperiments } /\ []
            ToggleSection section → F.noMessages $ model { section = section }
            RedirectKarma → model /\
                  [ do
                          liftEffect <<< FS.send imId <<< SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges
                          pure Nothing
                  ]
            UpdatePrivileges { privileges } → F.noMessages model { user { privileges = privileges } }