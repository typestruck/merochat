module Server.NotFound.Template where

import Shared.Extra as SX

import Prelude

import Flame as F

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Html (Html(..))

-- import Flame.Renderer.String as FRS

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters { content = content }
      -- Html <$> FRS.render contents
      pure $ Html ""

content ∷ ∀ message. Array (F.Html message)
content =
      [ HE.div [ HA.class' "green-box" ]
              [ HE.div [ HA.class' "error-page" ]
                      [ HE.div [ HA.class' "error-page-code" ] [ HE.text "404" ]
                      , HE.div_ [ HE.text "The requested page could not be found" ]
                      ]
              ]
      ]
