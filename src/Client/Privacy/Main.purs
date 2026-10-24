module Client.Privacy.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Shared.Element (ElementId(PrivacyRoot))

import Client.Navigation as CN
import Data.Void (Void)
import Flame (Application)
import Shared.External as SE
import Shared.External.Types (Page(..))
import Shared.Privacy (privacy)
import Shared.Help.View as SHV

application ∷ Application Unit Void
application =
      { model: unit
      , view: const $ SE.view CN.navigate PrivacyRoot $ [ SHV.view CN.navigate Privacy privacy ]
      , update: \model _ → model /\ []
      , subscribe: []
      }
