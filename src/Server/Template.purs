-- | Basic functions to compose templates.
module Server.Template where

import Prelude

import Data.String as DS
import Effect (Effect)
import Environment (commonJSHash, otherJSHash, baseCSSHash)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Options.File (imageBasePath)
import Shared.Path as SP
import Shared.Routes (routes)
import Shared.Types (ContentType(..))

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
      title: "MelanChat - Friendly Random Chat",
      favicon: imageBasePath <> "favicon.ico",
      javascript: [],
      css: [], --REFACTOR: should just be a list of file names
      content: [],
      footer: []
}

externalDefaultParameters :: forall a. Parameters a
externalDefaultParameters = defaultParameters {
      css = [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.pathery CSS "external.9361845d640d2fa7ac80"]
      ],
      content = [
            HE.div (HA.class' "header") [
                  HE.a [HA.href $ routes.landing {}, HA.class' "logo"] $
                        HE.img [
                              HA.createAttribute "srcset" $ DS.joinWith " " [SP.pathery PNG "logo-3-small", "180w,", SP.pathery PNG "logo", "250w,", SP.pathery PNG "logo-small", "210w"],
                              HA.createAttribute "sizes" "(max-width: 1365px) 180px, (max-width: 1919px) 210px, 250px",
                              HA.src $ SP.pathery PNG "logo"]
                        ]
      ],
      footer = [externalFooter]
}

template :: forall a. Parameters a -> Effect (Html a)
template = pure <<< templateWith

templateWith :: forall a. Parameters a -> Html a
templateWith parameters@{ title, content, css, footer, favicon } =
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
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href <<< SP.pathery CSS $ "base." <> baseCSSHash ]
            ]
            javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src <<< SP.pathery JS $ "other." <> otherJSHash],
                  HE.script' [HA.type' "text/javascript", HA.src <<< SP.pathery JS $ "common." <> commonJSHash]
            ] <> parameters.javascript

externalFooter :: forall a. Html a
externalFooter =
      HE.div (HA.class' "footer") [
            HE.a (HA.href $ routes.landing {}) <<< HE.img <<< HA.src $ SP.pathery PNG "logo-small",
            HE.ul (HA.class' "footer-menu") [
                  HE.li_ $ HE.a (HA.href $ routes.login.get {}) "Login",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#faq") "FAQ",
                  HE.li_ $ HE.a (HA.href $ routes.backer {}) "Become a backer",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#terms" ) "Terms and conditions",
                  HE.li_ $ HE.a (HA.href $ routes.help {} <> "#privacy") "Privacy police",
                  HE.li_ $ HE.a (HA.href "https://github.com/melanchat/melanchat") "Source code"
            ]
      ]
