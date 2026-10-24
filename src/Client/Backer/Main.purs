module Client.Backer.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Shared.Element (ElementId(ExternalBackerRoot))

import Client.Navigation as CN
import Flame (Application)
import Shared.Backer.View as SBV
import Shared.External as SE
import Shared.Backer.Types (BackerMessage)

application ∷ Application Unit BackerMessage
application =
      { model: unit
      , view: const $ SE.view CN.navigate ExternalBackerRoot [ SBV.view { visible: true } ]
      , update: \model _ → model /\ []
      , subscribe: []
      }
