module Server.Recover.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame as F
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Extra as SE
import Shared.Html (Html(..))
import Shared.Recover.View as SRV
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SR

-- import Flame.Renderer.String as FRS

template ∷ Maybe String → Effect Html
template token = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content token
            , javascript = javascript
            }
      -- Html <$> FRS.render contents
      pure $ Html ""
      where
      javascript =
            [ SE.script' [ HA.type' "text/javascript", HA.src $ SR.bundlePath Recover Js ]
            ]

content ∷ Maybe String → Array (F.Html Unit)
content token =
      [ void $ SRV.view
              { email: Nothing, password: Nothing, confirmation: Nothing, token, loading: false, error: Nothing, success: Nothing }
      ]
