-- | Basic functions to compose templates.
module Template where

import Hedwig as H
import Data.Unit(Unit)
import Hedwig(Html)
import Prelude(($))

type Html' = Html Unit
type Parameters = { javascript :: Html', css :: Html', content :: Html', footer :: Html'}

defaultParameters :: Parameters
defaultParameters = { javascript : H.script [] [], css : H.link [] [], content : H.div [] [], footer : H.footer [] [] }

template :: Parameters -> Html'
template { javascript : javascript, css : css, content : content, footer : footer } =
	H.html [H.lang "en"] [
		H.head [] [
			H.meta [H.charset "UTF-8"] [],
			H.meta [H.name "viewport", H.content "width=device-width, initial-scale=1.0"] [],
			H.link [H.rel "shortcut icon", H.type' "image/ico", H.href "/media/favicon.ico"] [],
			H.link [H.rel "stylesheet", H.type' "text/css", H.href "/css/base.css"] [],
			css,
			H.title' [] [H.text "MelanChat (friendly) random webchat"]
		],
		H.body [] [
			H.div [H.class' "content"] [
				H.main [H.id "main"] [content],
				H.div [H.id "loading", H.class' "loading"] []
			],
			footer,
			javascript
		]
	]

externalFooter :: Html'
externalFooter =
	H.footer [] [
		H.a [H.href "/"] [H.img [H.src "/media/logo.png"] []],
		H.ul [] [
			H.li [] [H.a [H.href "#"] [H.text "Help"]],
			H.li [] [H.a [H.href "https://github.com/azafeh/melanchat"] [H.text "Source code"]],
            H.li [] [H.a [H.href "#"] [H.text "Become a backer"]],
            H.li [] [H.a [H.href "/login"] [H.text "Login"]]
		]
	]

