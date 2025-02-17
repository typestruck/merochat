-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.Either (Either(..))
import Data.String as DS
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

type Parameters a =
      { title ∷ String
      , favicon ∷ String
      , javascript ∷ Array (Html a)
      , css ∷ Array (Html a)
      , content ∷ Array (Html a)
      , footer ∷ Array (Html a)
      , bundled ∷ Boolean
      }

defaultParameters ∷ ∀ a. Parameters a
defaultParameters =
      { title: "MeroChat - Friendly Random Chat"
      , favicon: SP.resourcePath (Left Favicon) Ico
      , javascript: []
      , css: []
      , --REFACTOR: should just be a list of file names
        content: []
      , footer: []
      , bundled: false
      }

--work around purs-backend-es bug
externalDefaultParameters ∷ ∀ a. Parameters a
externalDefaultParameters =
      { title: "MeroChat - Text only, no video or group chats, and friendly only random chat!"
      , favicon: SP.resourcePath (Left Favicon) Ico
      , javascript: []
      , bundled: false

      , css:
              [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath External Css ]
              ]
      , content:
              [ HE.div (HA.class' "header")
                      [ HE.a [ HA.href $ routes.landing {}, HA.class' "logo" ] $
                              HE.img
                                    [ HA.createAttribute "srcset" $ DS.joinWith " " [ SP.resourcePath (Left Logo3Small) Png, "180w,", SP.resourcePath (Left Logo) Png, "250w,", SP.resourcePath (Left LogoSmall) Png, "210w" ]
                                    , HA.createAttribute "sizes" "(max-width: 1365px) 180px, (max-width: 1919px) 210px, 250px"
                                    , HA.src $ SP.resourcePath (Left Logo) Png
                                    ]
                      ]
              ]
      , footer: [ externalFooter ]
      }

template ∷ ∀ a. Parameters a → Effect (Html a)
template = pure <<< templateWith

templateWith ∷ ∀ a. Parameters a → Html a
templateWith parameters@{ title, content, css, bundled, footer, favicon } =
      HE.html (HA.lang "en")
            [ HE.head_
                    ( [ HE.meta $ HA.charset "UTF-8"
                      , HE.meta [ HA.name "viewport", HA.content "width=device-width, initial-scale=1.0" ]
                      , HE.meta [ HA.name "description", HA.content "Chat to new people who also just want to chat. MeroChat is a text based chat site for having actual conversations" ]
                      , HE.link [ HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href favicon ]
                      , HE.link [ HA.rel "manifest", HA.href favicon, HA.href "/file/default/manifest.json" ]
                      , HE.title title
                      ] <> styleSheets
                    )
            , HE.body_ $ content <> footer <> javascript
            ]
      where
      styleSheets =
            ( if bundled then []
              else
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Base Css ]
                    ]
            ) <> css
      javascript =
            ( if bundled then []
              else
                    [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Common Js ]
                    ]
            ) <> [ HE.script [ HA.type' "text/javascript" ] "if (`serviceWorker` in navigator) navigator.serviceWorker.register(`/file/default/sw.js`);" ] <> parameters.javascript

externalFooter ∷ ∀ a. Html a
externalFooter =
      HE.div (HA.class' "footer")
            [ HE.a (HA.href $ routes.landing {}) <<< HE.img <<< HA.src $ SP.resourcePath (Left LogoSmall) Png
            , HE.ul (HA.class' "footer-menu")
                    [ HE.li_ $ HE.a (HA.href $ routes.login.get {}) "Login"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#faq") "FAQ"
                    , HE.li_ $ HE.a (HA.href $ routes.backer {}) "Become a backer"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#terms") "Terms and conditions"
                    , HE.li_ $ HE.a (HA.href $ routes.help {} <> "#privacy") "Privacy policy"
                    , HE.li_ $ HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] "Source code"
                    ]
            ]
