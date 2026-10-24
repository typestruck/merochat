module Server.Unsubscribe.Template where

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
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , title = "MeroChat - Unsubscribe from Emails"
            }
      -- Html <$> FRS.render contents
      pure $ Html ""

content ∷ ∀ message. Array (F.Html message)
content =
      [ HE.div [ HA.class' "pastel-area green-box" ]
              [ SX.h2 [ HA.class' "ext-heading" ] [ HE.text "Email unsubscribed" ]
              , SX.p_ [ HE.text "You have been unsubscribed. If you wish to subscribe again, log in and update your email settings" ]
              ]
      ]
