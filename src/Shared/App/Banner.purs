module Shared.App.Banner where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Shared.Extra as SE
import Flame.Html.Element as HE
import Shared.Routes (routesSpec)

banner ∷ ∀ message. Html message
banner = HE.div [ HA.class' "app-banner" ]
      [ SE.a [ HA.href $ routesSpec.app {} ] [ HE.text "MeroChat has an app! Tap here to install" ]
      ]
