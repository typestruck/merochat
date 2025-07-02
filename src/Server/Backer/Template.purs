module Server.Backer.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Backer.View as SBV
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP

template ∷ Effect String
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> [ SBV.view { visible: true } ]
            , css = externalDefaultParameters.css <>
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Backer Css ]
                    ]
            , title = "MeroChat - Become a backer"
            }
      FRS.render contents