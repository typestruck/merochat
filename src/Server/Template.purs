-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.App.Banner (banner)
import Shared.Extra as SE
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SR
import Shared.External as SPL

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
      , favicon: SR.resourcePath (Left Favicon) Ico
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
      , favicon: SR.resourcePath (Left Favicon) Ico
      , javascript: []
      , bundled: false
      , css: []
      , header:
              [ banner
              , SPL.header ""
              ,
                --, if production then SE.script' [ HA.type' "text/javascript", SE.innerHtml "666 theme-switcher.js 666" ] --used to inline theme switcher
                --else
                SE.script' [ HA.type' "text/javascript", HA.src $ "/file/default/theme-switcher.js" ]
              ]
      , content: []
      , footer:
              [ SPL.footer ]
      }

template ∷ ∀ a. Parameters a → Effect (Html a)
template = pure <<< templateWith

templateWith ∷ ∀ a. Parameters a → Html a
templateWith parameters =
      HE.html [ HA.lang "en" ]
            [ HE.head_
                    ( [ SE.meta [ HA.charset "UTF-8" ]
                      , SE.meta [ HA.name "viewport", HA.content "width=device-width, interactive-widget=resizes-content" ]
                      , SE.meta [ HA.name "description", HA.content "Text based, 1 on 1, friendly only random chat!" ]
                      , SE.link [ HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href parameters.favicon ]
                      , SE.link [ HA.rel "manifest", HA.href parameters.favicon, HA.href "/file/default/manifest.json" ]
                      , SE.link [ HA.rel "preconnect", HA.href "https://fonts.googleapis.com" ]
                      , SE.link [ HA.rel "preconnect", HA.href "https://fonts.gstatic.com", HA.createAttribute "crossorigin" "" ]
                      , SE.link [ HA.href "https://fonts.googleapis.com/css2?family=Alef:wght@400;700&family=Inter:ital,opsz@0,14..32;1,14..32&display=swap", HA.rel "stylesheet" ]
                      , HE.title [ HE.text parameters.title ]
                      ] <> styleSheets
                    )
            , HE.body_ $ parameters.header <> parameters.content <> parameters.footer <> javascript
            ]
      where
      styleSheets =
            ( if parameters.bundled then []
              else
                    [ SE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SR.bundlePath Base Css ]
                    ]
            ) <> parameters.css
      javascript =
            ( if parameters.bundled then []
              else
                    [ SE.script' [ HA.type' "text/javascript", HA.src $ SR.bundlePath Common Js ]
                    ]
            ) <> parameters.javascript
