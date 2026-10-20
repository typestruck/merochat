module Server.Login.Template where

import Prelude

import Effect (Effect)
import Flame as F
import Flame.Html.Attribute as HA
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Extra as SE
import Shared.Html (Html(..))
import Shared.Login.View as SLV
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SR

-- import Flame.Renderer.String as FRS

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , javascript = javascript
            , title = "MeroChat - Login"
            }
      -- Html <$> FRS.render contents
      pure $ Html ""
      where
      javascript =
            [ SE.script' [ HA.type' "text/javascript", HA.src $ SR.bundlePath Login Js ]
            ]

content ∷ Array (F.Html Unit)
content =
      [ SLV.view
              { setEmail: const unit, setPassword: const unit, submit: unit, keyPressed: const unit }
              { email: "", password: "", loading: false, error: "" }
      ]
