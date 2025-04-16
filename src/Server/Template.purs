-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.Either (Either(..))
import Data.String as DS
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.External.Svg as SLS
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

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
      { title: "MeroChat - Text only, no video or group chats, and friendly only random chat!"
      , favicon: SP.resourcePath (Left Favicon) Ico
      , javascript: []
      , bundled: false
      , css: []
      , header:
              [ HE.div (HA.class' "header")
                      [ HE.div [ HA.id "header", HA.class' "big-logo" ]
                              [ SLS.logo

                              ]
                      , HE.div (HA.class' "menu-merochat")
                              [ HE.div (HA.class' "menu")
                                      [ HE.a (HA.href $ routes.landing {}) "Home"
                                      , HE.a (HA.href $ routes.help {} <> "#faq") "FAQ"
                                      , HE.a (HA.href $ routes.backer {}) "Donate"
                                      , HE.a [ HA.href $ routes.login.get {}, HA.class' "login-link" ] "Login"
                                      , HE.div (HA.class' "theme-switcher")
                                              [ SLS.sun
                                              , SLS.moon
                                              ]
                                      ]
                              , HE.div (HA.class' "merochat")
                                      [ HE.h1 (HA.class' "name") "MeroChat"
                                      , HE.div (HA.class' "tagline") "Random chat without the sleaze"
                                      , HE.div (HA.class' "subtagline") "(Not a dating app!)"
                                      ]
                              ]
                      ]
              ]
      , content: []
      , footer:
              [ HE.div (HA.class' "footer")
                      [ HE.a (HA.href $ routes.help {} <> "#privacy") "Terms and conditions"
                      , HE.a (HA.href $ routes.help {} <> "#privacy") "Privacy policy"
                      , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] "Source code"
                      , HE.a (HA.href $ routes.backer {}) "Donate"
                      ]
              ]
      }

template ∷ ∀ a. Parameters a → Effect (Html a)
template = pure <<< templateWith

templateWith ∷ ∀ a. Parameters a → Html a
templateWith parameters =
      HE.html (HA.lang "en")
            [ HE.head_
                    ( [ HE.meta $ HA.charset "UTF-8"
                      , HE.meta [ HA.name "viewport", HA.content "width=device-width, initial-scale=1.0" ]
                      , HE.meta [ HA.name "description", HA.content "Chat to new people who also just want to chat. MeroChat is a text based chat site for having actual conversations" ]
                      , HE.link [ HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href parameters.favicon ]
                      , HE.link [ HA.rel "manifest", HA.href parameters.favicon, HA.href "/file/default/manifest.json" ]
                      , HE.title parameters.title
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
            ) <> [ HE.script [ HA.type' "text/javascript" ] "if (`serviceWorker` in navigator) navigator.serviceWorker.register(`/file/default/sw.js`);" ] <> parameters.javascript
