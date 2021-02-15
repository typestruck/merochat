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
            css = externalDefaultParameters.css <> [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SPT.pathery CSS "backer.5c2d5d65952114e0b0e3"]],
            content = externalDefaultParameters.content <> [SBV.view]
      }
      FRS.render contents