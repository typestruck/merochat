module Shared.Help.View where

import Prelude

import Effect (Effect)
import Flame (Html)
import Shared.Extra as SE
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.External.Types (Page(..))

view ∷ ∀ message. (Page → Effect Unit) → Page → Html message → Html message
view navigate selected content = HE.div [ HA.class' "public-content external-help" ]
      [ HE.div [ HA.class' "public-tabs" ]
              [ tab Help "FAQ"
              , tab Terms "Terms and conditions"
              , tab Privacy "Privacy policy"
              ]
      , HE.div [ HA.class' "help" ] [ content ]
      ]
      where
      tab page title = HE.div [ SE.onClick $ navigate page, HA.class' { "public-tab": true, selected: selected == page } ] [ HE.text title ]
