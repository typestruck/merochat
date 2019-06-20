module Server.Landing.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Shared.Template (defaultParameters, externalFooter)
import Shared.Template as ST

template :: Effect String
template = do
	contents <- ST.template defaultParameters { footer = externalFooter, content = content, javascript = javascript, css = css }
	FRS.render contents
	where   javascript = [
			HE.script' $ HA.src "https://www.google.com/recaptcha/api.js",
    			HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/landing.bundle.js"],
			--we need a global callback for grecaptha so we need to directly call the bundled code
			HE.script (HA.type' "text/javascript") "Landing.main()"
		]
		css = [
			HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/landing.css"]
		]
		content = [
			HE.div (HA.class' "landing") [
				HE.div (HA.class' "green-area") [
					HE.div (HA.class' "header") [
						HE.a [HA.href "/", HA.class' "logo"] "MelanChat",
						HE.div (HA.class' "login") $ HE.a (HA.href "/login") "Login"
					],
					HE.div [HA.class' "heading", HA.id "headline"] [
						HE.h1_ "Friendly. Random. Chat. #melanchat",
						HE.h4_ [
							HE.text "Create an account and chat right away with",
							HE.i_ " interesting ",
							HE.text "people"
						]
					],
					HE.div (HA.class' "sign-up") [
						HE.input [HA.type' "text", HA.id "email", HA.placeholder "Email"],
						HE.input [HA.type' "password", HA.id "password", HA.placeholder "Password"] ,
						HE.div' [HA.class' "g-recaptcha", HA.createAttribute "data-sitekey" "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", HA.id "captcha", HA.createAttribute "data-callback" "Landing.completeRegistration", HA.createAttribute "data-size" "invisible"],
						HE.input [HA.type' "button", HA.id "register", HA.value "Create account"]
					]
				]
			],
			HE.h2 (HA.class' "bold") "Looking to meet people online but all you get are creeps or bots? Look no further.",
			HE.div (HA.class' "blurb") [
				HE.div_ [
					HE.h2_ "Melanchat is different from other chat websites:",
					HE.ul (HA.class' "ul-vertical") [
						HE.li_ "No seedy people! Talk to folks who want to have meaningful conversations",
						HE.li_ "Safe, anonymous and viruses free",
						HE.li_ "Community driven: features are available based on how trusted an user is",
						HE.li_ "Random, but in a interesting way: matchs made by ultra fancy algorithms",
						HE.li_ "Full of watermelons"
					]
				],
				HE.div_ [
					HE.h2_ "This is how it works:",
					HE.p_ [
						HE.text "New users get a randomized account which needs",
						HE.b_ " karma ",
						HE.text "to access features and tools. Karma is earned by making great conversations, and being a good user. Meaning: creeps and unhelpful people are weeded out; interesting folks get more visibility.",
						HE.br,
						HE.text "Whenever you feel like chatting, the system matches you with a random user- and vice versa, ensuring no one gets ignored.",
						HE.br,
						HE.text "You decide what to share, who to share it with- want to take a break or delete your account forever? No worries. Your personal data will never be used for spam, harvesting or any shady dealings."
					]
				]
			],
				HE.div (HA.class' "orange-blurb") [
					HE.h2 (HA.class' "bold padding-gap") "Did you find other chat apps too simple or too quiet? MelanChat makes it so you will never be bored.",
					HE.div (HA.class' "blurb") [
						HE.div_ [
							HE.h2_ "MelanChat is feature rich:",
							HE.ul (HA.class' "ul-vertical") [
								HE.li_ "Send text, image and audio messages",
								HE.li_ "Set the level of privacy you are comfortable with",
								HE.li_ "Feeling uninspired? Let the app create your profile, or suggest what to say",
								HE.li_ "Get matched with people who actually reply",
								HE.li_ "Choose to take part in novel chat experiments"
							]
						],
						HE.div_ [
							HE.h2_ "...while still feeling cozy and private",
							HE.p_ [
								HE.text "From the source code, MelanChat is done with ",
								HE.a (HA.href "https://en.wikipedia.org/wiki/Smalltalk") "conversation",
								HE.text " in mind. Talk to people, have actual interactions and make real friendships without fearing the dreaded ASL questions or getting stalked.",
								HE.br,
								HE.text "MelanChat is not affiliated with Google, Facebook, Twitter; nor does it track or spy on you. It is also not a dating website- why upload duck face pictures when you can talk about the ",
								HE.a (HA.href "https://en.wikipedia.org/wiki/Dancing_plague_of_1518") "dancing plagues in the 16th century?",
								HE.br,
								HE.text "Did I mention watermelons are everywhere? It can't get better(or healthier) than that. 🍉🍉🍉"
							]
						]
					]
				],

			HE.div (HA.class' "red-call") [
				HE.a (HA.href "#headline") $ HE.h1_ "It is free! Click here to create an account and chat right away #melanchat"
			]
		]