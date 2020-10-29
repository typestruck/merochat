-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Routes (routes)

type Parameters a = {
      title :: String,
      favicon :: String,
      javascript :: Array (Html a),
      css :: Array (Html a),
      content :: Array (Html a),
      footer :: Array (Html a)
}

defaultParameters :: forall a. Parameters a
defaultParameters = {
      title: "MelanChat (Friendly) Random Chat",
      favicon: "/client/media/favicon.ico",
      javascript: [],
      css: [], --REFACTOR: should just be a list of file names
      content: [],
      footer: []
}

externalDefaultParameters :: forall a. Parameters a
externalDefaultParameters = defaultParameters {
      css = [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/external.css"]
      ],
      content = [
            HE.div (HA.class' "header") [
                  HE.a [HA.href $ routes.landing {}, HA.class' "logo"] $ HE.img [
                        HA.createAttribute "srcset" "/client/media/logo-2.png 350w, /client/media/logo.png 250w",
                        HA.createAttribute "sizes" "(min-width: 1299px) 350px, 250px",
                        HA.src "/client/media/logo.png"
                  ]
            ]
      ],
      footer = [externalFooter]
}

template :: forall a. Parameters a -> Effect (Html a)
template = pure <<< templateWith

templateWith :: forall a. Parameters a -> Html a
templateWith { title, content, css, footer, javascript, favicon } =
      HE.html (HA.lang "en") [
            HE.head_ ([
                  HE.meta $ HA.charset "UTF-8",
                  HE.meta [HA.name "viewport", HA.content "width=device-width, initial-scale=1.0"],
                  HE.link [HA.id "favicon", HA.rel "shortcut icon", HA.type' "image/ico", HA.href favicon],
                  HE.title title
            ] <> styleSheets <> css),
            HE.body_ $ content <> footer <> javascript
      ]
      where styleSheets = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/base.css"]
            ]

externalFooter :: forall a. Html a
externalFooter =
      HE.div (HA.class' "footer") [
            HE.a (HA.href $ routes.landing {}) <<< HE.img $ HA.src "/client/media/logo-small.png",
            HE.ul (HA.class' "footer-menu") [
                  HE.li_ $ HE.a (HA.href $ routes.login.get {}) "Login",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#faq") "FAQ",
                  HE.li_ $ HE.a (HA.href "#") "Become a backer",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#terms" ) "Terms and conditions",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#privacy") "Privacy police",
                  HE.li_ $ HE.a (HA.href "https://github.com/melanchat/melanchat") "Source code"
            ]
      ]
