-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Environment (production)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Svg as SS
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routesSpec)

type Parameters a =
      { title ∷ String
      , favicon ∷ String
      , javascript ∷ Array (Html a)
      , css ∷ Array (Html a)
      , header ∷ Array (Html a)
      , content ∷ Array (Html a)
      , footer ∷ Array (Html a)
      , bundled ∷ Boolean
      }

defaultParameters ∷ ∀ a. Parameters a
defaultParameters =
      { title: "MeroChat - Friendly Random Chat"
      , favicon: SP.resourcePath (Left Favicon) Ico
      , javascript: []
      , header: []
      , css: []
      , --REFACTOR: should just be a list of file names
        content: []
      , footer: []
      , bundled: false
      }

--work around purs-backend-es bug
externalDefaultParameters ∷ ∀ a. Parameters a
externalDefaultParameters =
      { title: "MeroChat - Text only, friendly random chat!"
      , favicon: SP.resourcePath (Left Favicon) Ico
      , javascript: []
      , bundled: false
      , css: []
      , header:
              [ HE.div [ HA.class' "header" ]
                      [ HE.div [ HA.id "header", HA.class' "big-logo" ]
                              [ SS.logo

                              ]
                      , HE.div [ HA.class' "menu-merochat" ]
                              [ HE.div [ HA.class' "menu" ]
                                      [ HE.a [ HA.href $ routesSpec.landing {} ] [ HE.text "Home" ]
                                      , HE.a [ HA.href $ routesSpec.help {} <> "#faq" ] [ HE.text "FAQ" ]
                                      , HE.a [ HA.href $ routesSpec.backer {} ] [ HE.text "Support us" ]
                                      , HE.a [ HA.href $ routesSpec.login.get {}, HA.class' "login-link" ] [ HE.text "Login" ]
                                      , HE.div [ HA.class' "theme-switcher" ]
                                              [ SS.sun []
                                              , SS.moon []
                                              ]
                                      ]
                              , HE.div [ HA.class' "merochat" ]
                                      [ HE.h1 [ HA.class' "name" ] [ HE.text "MeroChat" ]
                                      , HE.div [ HA.class' "tagline" ] [ HE.text "The friendliest place to chat on the internet" ]
                                      ]
                              ]
                      ]
              , if production then HE.script' [ HA.type' "text/javascript", HA.innerHtml "666 theme-switcher.js 666" ] --used to inline theme switcher
                else HE.script' [ HA.type' "text/javascript", HA.src $ "/file/default/theme-switcher.js" ]
              ]
      , content: []
      , footer:
              [ HE.div [ HA.class' "footer" ]
                      [ HE.a [ HA.href $ routesSpec.help {} <> "#privacy" ] [ HE.text "Terms and conditions" ]
                      , HE.a [ HA.href $ routesSpec.help {} <> "#privacy" ] [ HE.text "Privacy policy" ]
                      , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] [ HE.text "Source code" ]
                      , HE.a [ HA.href $ routesSpec.backer {} ] [ HE.text "Donate" ]
                      ]
              ]
      }

template ∷ ∀ a. Parameters a → Effect (Html a)
template = pure <<< templateWith

templateWith ∷ ∀ a. Parameters a → Html a
templateWith parameters =
      HE.html [ HA.lang "en" ]
            [ HE.head_
                    ( [ HE.meta [ HA.charset "UTF-8" ]
                      , HE.meta [ HA.name "viewport", HA.content "width=device-width, interactive-widget=resizes-content" ]
                      , HE.meta [ HA.name "description", HA.content "Text based, 1 on 1, friendly only random chat!" ]
                      , HE.link [ HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href parameters.favicon ]
                      , HE.link [ HA.rel "manifest", HA.href parameters.favicon, HA.href "/file/default/manifest.json" ]
                      , HE.link [ HA.rel "preconnect", HA.href "https://fonts.googleapis.com" ]
                      , HE.link [ HA.rel "preconnect", HA.href "https://fonts.gstatic.com", HA.createAttribute "crossorigin" "" ]
                      , HE.link [ HA.href "https://fonts.googleapis.com/css2?family=Alef:wght@400;700&family=Inter:ital,opsz@0,14..32;1,14..32&display=swap", HA.rel "stylesheet" ]
                      , HE.title [ HE.text parameters.title ]
                      ] <> styleSheets
                    )
            , HE.body_ $ parameters.header <> parameters.content <> parameters.footer <> javascript
            ]
      where
      styleSheets =
            ( if parameters.bundled then []
              else
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Base Css ]
                    ]
            ) <> parameters.css
      javascript =
            ( if parameters.bundled then []
              else
                    [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Common Js ]
                    ]
            ) <> parameters.javascript
