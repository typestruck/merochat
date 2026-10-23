module Shared.External where

import Prelude

import Shared.Theme as ST

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Extra as SE
import Shared.Routes (routesSpec)
import Shared.Svg (logo)
import Shared.Svg as SS

header ∷ ∀ message. String → Html message
header root =
      HE.div [ HA.class' "header" ]
          [ HE.div [ HA.id "header", HA.class' "big-logo" ]
                  [ logo ]
          , HE.div [ HA.class' "menu-merochat" ]
                  [ HE.div [ HA.class' "menu" ]
                          [ SE.a [ HA.href $ routesSpec.landing {} ] [ HE.text "Home" ]
                          , SE.a [ HA.href $ routesSpec.help {} <> "#faq" ] [ HE.text "FAQ" ]
                          , SE.a [ HA.href $ routesSpec.backer {} ] [ HE.text "Support us" ]
                          , SE.a [ HA.class' "login-link", HA.href $ routesSpec.login.get {} ] [ HE.text "Login" ]
                          , HE.div [ HA.class' "theme-switcher" ] [ SS.sun [ ST.event root "bindtap" ], SS.moon [ ST.event root "bindtap" ] ]
                          ]
                  , HE.div [ HA.class' "merochat" ]
                          [ HE.h1 [ HA.class' "name" ] [ HE.text "MeroChat" ]
                          , HE.div [ HA.class' "tagline" ] [ HE.text "The friendliest place to chat on the internet" ]
                          ]
                  ]
          ]

footer ∷ ∀ message. Html message
footer =
      HE.div [ HA.class' "footer" ]
          [ SE.a [ HA.href $ routesSpec.help {} <> "#privacy" ] [ HE.text "Terms and conditions" ]
          , SE.a [ HA.href $ routesSpec.help {} <> "#privacy" ] [ HE.text "Privacy policy" ]
          , SE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] [ HE.text "Source code" ]
          , SE.a [ HA.href $ routesSpec.backer {} ] [ HE.text "Donate" ]
          ]
