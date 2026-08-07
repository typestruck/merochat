module Shared.Experiments.Debate where

import Prelude hiding (join)

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (DebateSection(..), ExperimentsMessage(..), ExperimentsModel, PaperPlaneSection(..), PaperPlaneStatus(..))

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "paper-plane duller" ]
      [ HE.div [ HA.class' "green-tab" ]
              [ HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowNewDebate }, HA.onClick $ ToggleDebateSection ShowNewDebate ] [ HE.text "New" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowMine }, HA.onClick $ ToggleDebateSection ShowMine ] [ HE.text "Mine" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowJoin }, HA.onClick $ ToggleDebateSection ShowJoin ] [ HE.text "Join" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowPublic }, HA.onClick $ ToggleDebateSection ShowPublic ] [ HE.text "Public" ]
              ]
      , case model.debate.section of
              ShowNewDebate → new model
              ShowMine -> mine model
              ShowJoin → join model
              ShowPublic → public model
      ]

new model = HE.div [] [HE.text "Currently unavailable"]

mine model = HE.div [] [HE.text "Currently unavailable"]

join model = HE.div [] [HE.text "Currently unavailable"]

public model = HE.div [] [HE.text "Currently unavailable"]