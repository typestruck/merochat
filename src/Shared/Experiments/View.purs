module Shared.Experiments.View where

import Prelude
import Shared.Experiments.Types

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Impersonation as SEI
import Shared.Experiments.WordChain as SEW
import Shared.Experiments.Doppelganger as SED

view ∷ ChatExperimentModel → Html ChatExperimentMessage
view model = HE.div [HA.id "chat-experiments", HA.class' { hidden: not model.visible }] $ case model.current of
      Just _ →
            HE.div (HA.class' "modal-section") "joined" -- $ SEI.joined profile
      _ →
            HE.div (HA.class' "modal-section") $ map toDiv model.experiments
      where
      toDiv experiment = HE.div (HA.class' "modal-part")
            [ HE.div (HA.class' "section-label")
                    [ HE.label (HA.class' "bold") experiment.name
                    , HE.div (HA.class' "duller experiment-description") experiment.description
                    ]
            , HE.fragment $ extra model experiment.code
            ]

extra ∷ ChatExperimentModel → Experiment → Html ChatExperimentMessage
extra model = case _ of
      Impersonation ip → SEI.view model
      WordChain → SEW.view model
      Doppelganger → SED.view model