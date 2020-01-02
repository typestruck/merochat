-- | Basic functions to compose templates.
module Server.Template(
        template,
        externalFooter,
        templateWith,
        extendParameters,
        defaultParameters,
        externalDefaultParameters
) where

import Data.Array ((:))
import Effect (Effect)
import Flame (Html)
import Flame.HTML.Element as HE
import Flame.HTML.Attribute as HA
import Prelude
import Effect.Now as EN
import Data.Time(Time(..))
import Data.Enum as DE

--TODO memoization, caching

type Parameters a = {
        javascript :: Array (Html a),
        css :: Array (Html a),
        content :: Array (Html a),
        footer :: Array (Html a),
        nightTheme :: Boolean
}

defaultParameters :: forall a. Parameters a
defaultParameters = {
        javascript: [],
        css: [],
        content: [],
        footer: [],
        nightTheme: false
}

externalDefaultParameters :: forall a. Parameters a
externalDefaultParameters = {
        javascript: [],
        css: [
                HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/external.css"]
        ],
        content: [
                HE.div (HA.class' "header") [
                        HE.a [HA.href "/", HA.class' "logo"] "MelanChat",
                        HE.div (HA.class' "login") $ HE.a (HA.href "/login") "Login"
                ]
        ],
        footer: externalFooter,
        nightTheme: false
}

--inclusion of night theme should be a setting
template :: forall a. Parameters a -> Effect (Html a)
template parameters = do
        parameters' <- extendParameters parameters
        pure $ templateWith parameters'

extendParameters :: forall a. Parameters a -> Effect (Parameters a)
extendParameters parameters = do
        Time hour _ _ _ <- EN.nowTime
        pure $ parameters {nightTheme = true}-- (DE.fromEnum hour) >= 17 && (DE.fromEnum hour) <= 7}

templateWith :: forall a. Parameters a -> Html a
templateWith parameters =
        HE.html (HA.lang "en") [
                HE.head_ ([
                        HE.meta $ HA.charset "UTF-8",
                        HE.meta [HA.name "viewport", HA.content "width=device-width, initial-scale=1.0"],
                        HE.link [HA.rel "shortcut icon", HA.type' "image/ico", HA.href "/client/media/favicon.ico"],
                        HE.title "MelanChat (friendly) random webchat"
                ] <> parameters.css <> styleSheets),
                HE.body_ (HE.div' [HA.id "loading", HA.class' "loading"] : parameters.content <> parameters.footer <> parameters.javascript)
        ]
        where defaultCss = [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/base.css"]]
              styleSheets = if parameters.nightTheme then defaultCss <> [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/night.css"]] else defaultCss

externalFooter :: forall a. Array (Html a)
externalFooter = [
        HE.footer_ [
                HE.a (HA.href "/") <<< HE.img' $ HA.src "/client/media/logo.png",
                HE.ul_ [
                        HE.li_ $ HE.a (HA.href "#") "Help",
                        HE.li_ $ HE.a (HA.href "https://github.com/azafeh/melanchat") "Source code",
                            HE.li_ $ HE.a (HA.href "#") "Become a backer",
                            HE.li_ $ HE.a (HA.href "/login") "Login"
                ]
        ]
]
