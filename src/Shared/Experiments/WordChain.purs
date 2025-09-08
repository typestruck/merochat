module Shared.Experiments.WordChain where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (ChatExperimentMessage, ChatExperimentModel)

view ∷ ChatExperimentModel → Html ChatExperimentMessage
view model = HE.div [HA.class' "word-chain duller"]
      [ --HE.button [HA.class' "green-button"] [HE.text "Play!"]
        HE.text "Currently unavailable"
      ]