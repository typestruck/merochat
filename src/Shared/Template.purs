-- | Basic functions to compose templates.
module Template(template, externalFooter, defaultParameters) where

import Data.Array ((:))
import Data.Unit (Unit)
import Effect (Effect)
import Hedwig (Html)
import Hedwig as H
import Prelude ((<>), bind, ($), pure, (>=), (<=), (&&), discard, show)
import Effect.Now as E
import Effect.Console
import Effect.Class(liftEffect)
import Data.Time(Time(..))
import Data.Enum(fromEnum)

--TODO memoization, caching

type Html' = Html Unit
type Parameters = { javascript :: Array Html', css :: Array Html', content :: Array Html', footer :: Array Html', nightTheme :: Boolean}

defaultParameters :: Parameters
defaultParameters = { javascript : [], css : [], content : [], footer : [], nightTheme : false }

--inclusion of night theme should be a setting
template :: Parameters -> Effect Html'
template parameters = do
	Time hour _ _ _ <- E.nowTime
	pure $ templateWith parameters {nightTheme = false}-- (fromEnum hour) >= 17 && (fromEnum hour) <= 7}

templateWith :: Parameters -> Html'
templateWith parameters =
	H.html [H.lang "en"] [
		H.head [] ([
			H.meta [H.charset "UTF-8"] [],
			H.meta [H.name "viewport", H.content "width=device-width, initial-scale=1.0"] [],
			H.link [H.rel "shortcut icon", H.type' "image/ico", H.href "/client/media/favicon.ico"] [],
			H.title' [] [H.text "MelanChat (friendly) random webchat"]
		] <> styleSheets <> parameters.css),
		H.body [] (H.div [H.id "loading", H.class' "loading"] [] : parameters.content <> parameters.footer <> parameters.javascript)
	]
	where defaultCss = [H.link [H.rel "stylesheet", H.type' "text/css", H.href "/client/css/base.css"] []]
	      styleSheets = if parameters.nightTheme then defaultCss <> [H.link [H.rel "stylesheet", H.type' "text/css", H.href "/client/css/night.css"] []] else defaultCss

externalFooter :: Array Html'
externalFooter = [
	H.footer [] [
		H.a [H.href "/"] [H.img [H.src "/client/media/logo.png"] []],
		H.ul [] [
			H.li [] [H.a [H.href "#"] [H.text "Help"]],
			H.li [] [H.a [H.href "https://github.com/azafeh/melanchat"] [H.text "Source code"]],
            H.li [] [H.a [H.href "#"] [H.text "Become a backer"]],
            H.li [] [H.a [H.href "/login"] [H.text "Login"]]
		]
	]
]
