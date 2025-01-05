module Client.Experiments.Update where

import Prelude
import Shared.Experiments.Types

import Client.Common.Dom (setChatExperiment)
import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame (ListUpdate)
import Flame as F
import Flame.Subscription as FS
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Shared.Im.Types (ImMessage(..), RetryableRequest(..), ShowUserMenuModal(..))
import Shared.Options.MountPoint (imId)

update ∷ ListUpdate ChatExperimentModel ChatExperimentMessage
update model =
      case _ of
            QuitExperiment → model /\
                  [ do
                          liftEffect CCL.reload
                          pure Nothing
                  ]
            JoinExperiment code →
                  F.noMessages model
            -- { current = Just code
            -- } /\ dispatchEvent (Just code)
            ToggleSection section → F.noMessages $ model { section = section }
            ConfirmExperiment experiment → F.noMessages model { confirming = experiment }
            RedirectKarma → model /\
                  [ do
                          liftEffect <<< FS.send imId <<< SpecialRequest $ ToggleModal ShowKarmaPrivileges
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