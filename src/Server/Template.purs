-- | Basic functions to compose templates.
module Server.Template where

import Prelude
import Shared.Types

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Routes (routes)


--TODO memoization, caching -- it would be nice to serve this as static files?

type Parameters a = {
      javascript :: Array (Html a),
      css :: Array (Html a),
      content :: Array (Html a),
      footer :: Array (Html a)
}

defaultParameters :: forall a. Parameters a
defaultParameters = {
      javascript: [],
      css: [], --REFACTOR: should just be a list of file names
      content: [],
      footer: []
}

externalDefaultParameters :: forall a. Parameters a
externalDefaultParameters = {
      javascript: [],
      css: [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/external.css"]
      ],
      content: [
            HE.div (HA.class' "header") [
                  HE.a [HA.href $ routes.landing {}, HA.class' "logo"] $ HE.img [
                              HA.createAttribute "srcset" "/client/media/logo-2.png 350w, /client/media/logo.png 250w",
                              HA.createAttribute "sizes" "(min-width: 1299px) 350px, 250px",
                              HA.src "/client/media/logo.png"]
            ]
      ],
      footer: [externalFooter]
}

template :: forall a. Parameters a -> Effect (Html a)
template = pure <<< templateWith

templateWith :: forall a. Parameters a -> Html a
templateWith parameters =
      HE.html (HA.lang "en") [
            HE.head_ ([
                  HE.meta $ HA.charset "UTF-8",
                  HE.meta [HA.name "viewport", HA.content "width=device-width, initial-scale=1.0"],
                  HE.link [HA.rel "shortcut icon", HA.type' "image/ico", HA.href "/client/media/favicon.ico"],
                  HE.title "MelanChat (Friendly) Random Chat"
            ] <> styleSheets <> parameters.css),
            HE.body_ $ parameters.content <> parameters.footer <> parameters.javascript
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
