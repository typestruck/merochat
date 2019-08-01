module Server.Login.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Shared.Template (defaultParameters, externalFooter)
import Shared.Template as ST

template :: Effect String
template = do
	contents <- ST.template defaultParameters {
		footer = externalFooter,
		content = content,
		javascript = javascript,
		css = css
	}
	FRS.render contents
	where   css = [
			HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/external.css"]
		]
		javascript = [
			HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/login.bundle.js"]
		]
		content = [
			HE.div (HA.class' "center-box") [
				HE.h1_ "Login to MelanChat",
				HE.div (HA.class' "box-action") [
					HE.input [HA.id "email", HA.type' "text", HA.placeholder "Email"],
					HE.input [HA.id "password", HA.type' "password", HA.placeholder "Password"],
					HE.input [HA.id "login", HA.type' "button", HA.value "Login"]
				],
				HE.ul_ [
					HE.li_ $ HE.a [HA.href "/"] "Don't have an account? Register",
					HE.li_ $ HE.a [HA.href "/recover"] "Forgot your password? Recover"
				]
			]
		]