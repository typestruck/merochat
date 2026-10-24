module Client.Terms.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Shared.Element (ElementId(TermsRoot))

import Client.Navigation as CN
import Data.Void (Void)
import Flame (Application)
import Shared.External as SE
import Shared.External.Types (Page(..))
import Shared.Terms as ST
import Shared.Help.View as SHV

application ∷ Application Unit Void
application =
      { model: unit
      , view: const $ SE.view CN.navigate TermsRoot $ [ SHV.view CN.navigate Terms $ ST.termsWithNavigation $ CN.navigate Privacy ]
      , update: \model _ → model /\ []
      , subscribe: []
      }
