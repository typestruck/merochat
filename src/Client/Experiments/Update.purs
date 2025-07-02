module Client.Experiments.Update where

import Prelude
import Shared.Experiments.Types

import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame (ListUpdate)
import Flame as F
import Flame.Subscription as FS
import Shared.Im.Types (ImMessage(..), RetryableRequest(..))
import Shared.Modal.Types (Modal(..), ScreenModal(..))
import Shared.Options.MountPoint (imId)

update ∷ ListUpdate ChatExperimentModel ChatExperimentMessage
update model =
      case _ of
            QuitExperiment → model /\
                  [ do
                          liftEffect CCL.reload
                          pure Nothing
                  ]
            ToggleVisibility modal → model { visible = modal == ShowExperiments } /\ []
            JoinExperiment code →
                  F.noMessages model
            -- { current = Just code
            -- } /\ dispatchEvent (Just code)
            ToggleSection section → F.noMessages $ model { section = section }
            ConfirmExperiment experiment → F.noMessages model { confirming = experiment }
            RedirectKarma → model /\
                  [ do
                          liftEffect <<< FS.send imId <<< SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges
                          pure Nothing
                  ]
            UpdatePrivileges { privileges } → F.noMessages model { user { privileges = privileges } }
-- where
-- dispatchEvent payload =
--       [ liftEffect do
--               --refactor: if experiments depends on im on webpack this can be safe
--               FSUC.broadcast setChatExperiment payload
--               pure Nothing
--       ]