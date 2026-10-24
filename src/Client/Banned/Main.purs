module Client.Banned.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Shared.Element (ElementId(BannedRoot))

import Client.Navigation as CN
import Data.Void (Void)
import Flame (Application)
import Shared.External as SE
import Server.Banned.Template as SBT

application ∷ Application Unit Void
application =
      { model: unit
      , view: const $ SE.view CN.navigate BannedRoot $ SBT.content
      , update: \model _ → model /\ []
      , subscribe: []
      }
