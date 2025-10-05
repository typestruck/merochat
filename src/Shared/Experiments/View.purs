module Shared.Experiments.View where

import Prelude
import Shared.Experiments.Types

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.WordChain as SEW
import Shared.Experiments.Doppelganger as SED

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.id "chat-experiments", HA.class' { hidden: not model.visible } ]
      [ HE.div [ HA.class' "modal-section" ] $ map toDiv model.experiments
      ]
      where
      toDiv experiment = HE.div [ HA.class' "modal-part" ]
            [ HE.div [ HA.class' "section-label" ]
                    [ HE.label [ HA.class' "bold" ] [ HE.text experiment.name ]
                    , HE.div [ HA.class' "duller experiment-description" ] [ HE.text experiment.description ]
                    ]
            , HE.fragment [ extra model experiment.code ]
            ]

extra ∷ ExperimentsModel → Experiment → Html ExperimentsMessage
extra model = case _ of
      WordChain → SEW.view model
      Doppelganger → SED.view model