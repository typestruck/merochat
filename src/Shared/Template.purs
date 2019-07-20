-- | Basic functions to compose templates.
module Shared.Template(template, externalFooter, defaultParameters) where

import Data.Array ((:))
import Data.Unit (Unit)
import Effect (Effect)
import Flame (Html)
import Flame.HTML.Element as HE
import Flame.HTML.Attribute as HA
import Prelude
import Effect.Now as EN
import Data.Time(Time(..))
import Data.Enum as DE

--TODO memoization, caching

type Html' = Html Unit
type Parameters = { javascript :: Array Html', css :: Array Html', content :: Array Html', footer :: Array Html', nightTheme :: Boolean}

defaultParameters :: Parameters
defaultParameters = { javascript : [], css : [], content : [], footer : [], nightTheme : false }

--inclusion of night theme should be a setting
template :: Parameters -> Effect Html'
template parameters = do
	Time hour _ _ _ <- EN.nowTime
	pure $ templateWith parameters {nightTheme = true}-- (DE.fromEnum hour) >= 17 && (DE.fromEnum hour) <= 7}

templateWith :: Parameters -> Html'
templateWith parameters =
	HE.html (HA.lang "en") [
		HE.head_ ([
			HE.meta $ HA.charset "UTF-8",
			HE.meta [HA.name "viewport", HA.content "width=device-width, initial-scale=1.0"],
			HE.link [HA.rel "shortcut icon", HA.type' "image/ico", HA.href "/client/media/favicon.ico"],
			HE.title "MelanChat (friendly) random webchat"
		] <> styleSheets <> parameters.css),
		HE.body_ (HE.div' [HA.id "loading", HA.class' "loading"] : parameters.content <> parameters.footer <> parameters.javascript)
	]
	where defaultCss = [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/base.css"]]
	      styleSheets = if parameters.nightTheme then defaultCss <> [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/night.css"]] else defaultCss

externalFooter :: Array Html'
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
