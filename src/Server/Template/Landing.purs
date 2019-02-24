module Template.Landing where

import Hedwig as H
import Template as T
import Template(defaultParameters, externalFooter)
import Effect(Effect)
import Prelude(($), bind)

landing :: Effect String
landing = do
	contents <- T.template defaultParameters { footer = externalFooter, content = content, javascript = javascript, css = css }
	H.render $ contents
	where   javascript = [
			H.script [H.src "https://www.google.com/recaptcha/api.js"] [],
    			H.script [H.type' "text/javascript", H.src "/client/javascript/landing.bundle.js"] [],
			--we need a global callback for grecaptha so we need to directly call the bundled code
			H.script [H.type' "text/javascript"] [H.text "Landing.main()"]
		]
		css = [
			H.link [H.rel "stylesheet", H.type' "text/css", H.href "/client/css/landing.css"] []
		]
		content = [
		H.div [H.class' "landing"] [
	        	H.div [H.class' "green-area"] [
		                H.div [H.class' "header"] [
		                	H.a [H.href "/", H.class' "logo"] [H.text "MelanChat"],
		                    	H.div [H.class' "login"] [H.a [H.href "/login"] [H.text "Login"]]
				],
		                H.div [H.class' "heading", H.id "headline"] [
			        	H.h1 [] [H.text "Friendly. Random. Chat. #melanchat"],
			            	H.h4 [] [
						    H.text "Create an account and chat right away with",
						    H.i [] [H.text " interesting "],
						    H.text "people"
					]
		                ],
		                H.div [H.class' "sign-up"] [
		                	H.input [H.type' "text", H.id "email", H.placeholder "Email"] [],
		                    	H.input [H.type' "password", H.id "password", H.placeholder "Password"] [],
		                    	H.div [H.class' "g-recaptcha", H.property "data-sitekey" "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", H.id "captcha", H.property "data-callback" "Landing.completeRegistration", H.property "data-size" "invisible"] [],
		                    	H.input [H.type' "button", H.id "register", H.value "Create account"] []
				]
			]
		],
	        H.h2 [H.class' "bold"] [H.text "Looking to meet people online but all you get are creeps or bots? Look no further."],
	        H.div [H.class' "blurb"] [
	                H.div  [] [
	                	H.h2 [] [H.text "Melanchat is different from other chat websites:"],
	                    	H.ul [H.class' "ul-vertical"] [
	                        	H.li [] [H.text "No seedy people! Talk to folks who want to have meaningful conversations"],
		                        H.li [] [H.text "Safe, anonymous and viruses free"],
		                        H.li [] [H.text "Community driven: features are available based on how trusted an user is"],
		                        H.li [] [H.text "Random, but in a interesting way: matchs made by ultra fancy algorithms"],
		                        H.li [] [H.text "Full of watermelons"]
			        ]
			],
	                H.div [] [
	                    	H.h2 [] [H.text "This is how it works:"],
	                    	H.p [] [
			    		H.text "New users get a randomized account which needs",
	                        	H.b [] [H.text " karma "],
					H.text "to access features and tools. Karma is earned by making great conversations, and being a good user. Meaning: creeps and unhelpful people are weeded out; interesting folks get more visibility.",
	                        	H.br [] [],
					H.text "Whenever you feel like chatting, the system matches you with a random user- and vice versa, ensuring no one gets ignored.",
		                        H.br [] [],
					H.text "You decide what to share, who to share it with- want to take a break or delete your account forever? No worries. Your personal data will never be used for spam, harvesting or any shady dealings."
	                        ]
			]
		],
	            	H.div [H.class' "orange-blurb"] [
	                	H.h2 [H.class' "bold padding-gap"] [H.text "Did you find other chat apps too simple or too quiet? MelanChat makes it so you will never be bored."],
	                	H.div [H.class' "blurb"] [
		                    	H.div [] [
		                        	H.h2 [] [H.text "MelanChat is feature rich:"],
			                        H.ul [H.class' "ul-vertical"] [
			                        	H.li [] [H.text "Send text, image and audio messages"],
			                            	H.li [] [H.text "Set the level of privacy you are comfortable with"],
			                            	H.li [] [H.text "Feeling uninspired? Let the app create your profile, or suggest what to say"],
			                            	H.li [] [H.text "Get matched with people who actually reply"],
			                            	H.li [] [H.text "Choose to take part in novel chat experiments"]
						]
					],
		                    	H.div [] [
		                        	H.h2 [] [H.text "...while still feeling cozy and private"],
		                        	H.p [] [
							H.text "From the source code, MelanChat is done with ",
							H.a [H.href "https://en.wikipedia.org/wiki/Smalltalk"] [H.text "conversation"],
							H.text " in mind. Talk to people, have actual interactions and make real friendships without fearing the dreaded ASL questions or getting stalked.",
							H.br [] [],
							H.text "MelanChat is not affiliated with Google, Facebook, Twitter; nor does it track or spy on you. It is also not a dating website- why upload duck face pictures when you can talk about the ",
							H.a [H.href "https://en.wikipedia.org/wiki/Dancing_plague_of_1518"] [H.text "dancing plagues in the 16th century?"],
							H.br [] [],
		                        		H.text "Did I mention watermelons are everywhere? It can't get better(or healthier) than that. 🍉🍉🍉"
						]
					]
				]
			],

		H.div [H.class' "red-call"] [
	               	H.a [H.href "#headline"] [H.h1 [] [H.text "It is free! Create an account and start chatting right away #melanchat"]]
		]
	]