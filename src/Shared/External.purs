module Shared.External where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Element (ElementId)
import Shared.External.Types (Page(..))
import Shared.Extra as SE
import Shared.Routes (routesSpec)
import Shared.Svg (logo)
import Shared.Svg as SS
import Shared.Theme as ST

header ∷ ∀ message. String → Html message
header = headerWith $ const []

headerWith ∷ ∀ message. (Page → Array (NodeData message)) → String → Html message
headerWith events root =
      HE.div [ HA.class' "header" ]
            [ HE.div [ HA.id "header", HA.class' "big-logo" ]
                    [ logo ]
            , HE.div [ HA.class' "menu-merochat" ]
                    [ HE.div [ HA.class' "menu" ]
                            [ SE.a ([ HA.href $ routesSpec.landing {} ] <> events Landing) [ HE.text "Home" ]
                            , SE.a ([ HA.href $ routesSpec.help {} <> "#faq" ] <> events Help) [ HE.text "FAQ" ]
                            , SE.a ([ HA.href $ routesSpec.backer {} ] <> events Backer) [ HE.text "Support us" ]
                            , SE.a ([ HA.class' "login-link", HA.href $ routesSpec.login.get {} ] <> events Login) [ HE.text "Login" ]
                            , HE.div [ HA.class' "theme-switcher" ] [ SS.sun [ ST.runThemeEvent root "bindtap" ], SS.moon [ ST.runThemeEvent root "bindtap" ] ]
                            ]
                    , HE.div [ HA.class' "merochat" ]
                            [ HE.h1 [ HA.class' "name" ] [ HE.text "MeroChat" ]
                            , HE.div [ HA.class' "tagline" ] [ HE.text "The friendliest place to chat on the internet" ]
                            ]
                    ]
            ]

footer ∷ ∀ message. Html message
footer = footerWith $ const []

footerWith ∷ ∀ message. (Page → Array (NodeData message)) → Html message
footerWith handleLink =
      HE.div [ HA.class' "footer" ]
            [ SE.a ([ HA.href $ routesSpec.help {} <> "#terms" ] <> handleLink Terms) [ HE.text "Terms and conditions" ]
            , SE.a ([ HA.href $ routesSpec.help {} <> "#privacy" ] <> handleLink Privacy) [ HE.text "Privacy policy" ]
            , SE.a [ HA.href "https://github.com/typestruck/merochat", SE.externalLink "https://github.com/typestruck/merochat", HA.target "_blank" ] [ HE.text "Source code" ]
            , SE.a ([ HA.href $ routesSpec.backer {} ] <> handleLink Backer) [ HE.text "Donate" ]
            ]

view ∷ ∀ message. (Page → Effect Unit) → ElementId → Array (Html message) → Html message
view navigate root content =
      HE.scrollView
            [ HA.id $ show root
            , ST.onLayoutChange $ show root
            , HA.class' "public-route"
            , HA.scrollOrientation "vertical"
            ]
            $ headerWith (pure <<< SE.onClick <<< navigate) (show root) : content <> [ footerWith $ pure <<< SE.onClick <<< navigate ]
