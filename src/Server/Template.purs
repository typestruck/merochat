-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.String as DS
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Resource (Resource(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

type Parameters a =
      { title ∷ String
      , favicon ∷ String
      , javascript ∷ Array (Html a)
      , css ∷ Array (Html a)
      , content ∷ Array (Html a)
      , footer ∷ Array (Html a)
      , bundled :: Boolean
      }

defaultParameters ∷ ∀ a. Parameters a
defaultParameters =
      { title: "MelanChat - Friendly Random Chat"
      , favicon: SP.resourcePath Favicon Ico
      , javascript: []
      , css: []
      , --REFACTOR: should just be a list of file names
        content: []
      , footer: []
      , bundled: false
      }

externalDefaultParameters ∷ ∀ a. Parameters a
externalDefaultParameters = defaultParameters
      { css =
              [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.resourcePath External Css]
              ]
      , content =
              [ HE.div (HA.class' "header")
                      [ HE.a [ HA.href $ routes.landing {}, HA.class' "logo" ] $
                              HE.img
                                    [ HA.createAttribute "srcset" $ DS.joinWith " " [ SP.resourcePath Logo3Small Png, "180w,", SP.resourcePath Logo Png, "250w,", SP.resourcePath LogoSmall Png, "210w" ]
                                    , HA.createAttribute "sizes" "(max-width: 1365px) 180px, (max-width: 1919px) 210px, 250px"
                                    , HA.src $ SP.resourcePath Logo Png
                                    ]
                      ]
              ]
      , footer = [ externalFooter ]
      }

template ∷ ∀ a. Parameters a → Effect (Html a)
template = pure <<< templateWith

templateWith ∷ ∀ a. Parameters a → Html a
templateWith parameters@{ title, content, css, bundled, footer, favicon } =
      HE.html (HA.lang "en")
            [ HE.head_
                    ( [ HE.meta $ HA.charset "UTF-8"
                      , HE.meta [ HA.name "viewport", HA.content "width=device-width, initial-scale=1.0" ]
                      , HE.link [ HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href favicon ]
                      , HE.title title
                      ] <> styleSheets <> css
                    )
            , HE.body_ $ content <> footer <> javascript
            ]
      where
      styleSheets =
            [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.resourcePath Base Css ]
            ]
      javascript =
            (if bundled then [] else [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.resourcePath Common Js ]
            ]) <> parameters.javascript

externalFooter ∷ ∀ a. Html a
externalFooter =
      HE.div (HA.class' "footer")
            [ HE.a (HA.href $ routes.landing {}) <<< HE.img <<< HA.src $ SP.resourcePath LogoSmall Png
            , HE.ul (HA.class' "footer-menu")
                    [ HE.li_ $ HE.a (HA.href $ routes.login.get {}) "Login"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#faq") "FAQ"
                    , HE.li_ $ HE.a (HA.href $ routes.backer {}) "Become a backer"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#terms") "Terms and conditions"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#privacy") "Privacy police"
                    , HE.li_ $ HE.a (HA.href "https://github.com/melanchat/melanchat") "Source code"
                    ]
            ]
