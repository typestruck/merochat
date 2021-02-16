module Server.Backer.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Path as SPT
import Shared.Types (ContentType(..))
import Shared.Backer.View as SBV

template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters {
            content = externalDefaultParameters.content <> [SBV.view]
      }
      FRS.render contents