module Server.Landing.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters, externalFooter)
import Server.Template as ST



template :: Effect String
template = do
      contents <- ST.template defaultParameters {
            content = content,
            javascript = javascript,
            css = css
      }
      FRS.render contents
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/landing.bundle.js"],
                  HE.script' $ HA.src "https://www.google.com/recaptcha/api.js"
            ]
            css = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/external.css"],
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/landing.css"]
            ]
            content = [
                  HE.div (HA.class' "landing") [
                        HE.div (HA.class' "header") [
                              HE.a [HA.href $ "", HA.class' "logo"] $ HE.img [
                                          HA.createAttribute "srcset" "/client/media/logo.png 250w, /client/media/logo-small.png 210w",
                                          HA.createAttribute "sizes" "(max-width: 1599px) 210px, 250px",
                                          HA.src "/client/media/logo.png"],
                              HE.div [HA.class' "login-box"] $ HE.a [HA.class' "login", HA.href ""] "Login"
                        ],
                        HE.div (HA.class' "green-area") [
                              HE.h1 (HA.id "headline") "Friendly. Random. Chat.",
                              HE.h4 (HA.class' "subheadline") [
                                    HE.text "Create an account and chat right away with",
                                    HE.i_ " interesting ",
                                    HE.text "people"
                              ],
                              HE.div (HA.class' "form-up") [
                                    HE.input [HA.type' "text", HA.id "email", HA.placeholder "Email"],
                                    HE.input [HA.type' "password", HA.autocomplete false, HA.id "password", HA.placeholder "Password"] ,
                                    HE.div' [HA.class' "g-recaptcha", HA.createAttribute "data-sitekey" "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", HA.id "captcha", HA.createAttribute "data-callback" "completeRegistration", HA.createAttribute "data-size" "invisible"],
                                    HE.input [HA.type' "button", HA.id "register", HA.value "Create account"]
                              ]
                        ],
                        HE.h2_ [
                              HE.text "MelanChat is made with human connection and",
                              HE.br,
                              HE.text "conversation in mind"
                        ],
                        HE.div (HA.class' "first-points") [
                              HE.div (HA.class' "point-column") [
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point1.png"],
                                          HE.br,
                                          HE.text "No seedy people!",
                                          HE.br,
                                          HE.text "Talk to folks who want to",
                                          HE.br,
                                          HE.text "have meaningful conversations"
                                    ],
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point4.png"],
                                          HE.br,
                                          HE.text "Community driven:",
                                          HE.br,
                                          HE.text "feature available based on",
                                          HE.br,
                                          HE.text "how trusted an user is"
                                    ]
                              ],
                              HE.div (HA.class' "point-column") [
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point2.png"],
                                          HE.br,
                                          HE.text "Set the privacy level you",
                                          HE.br,
                                          HE.text "feel confortable with.",
                                          HE.br,
                                          HE.text "Safe, anonymous and ad free"
                                    ],
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point5.png"],
                                          HE.br,
                                          HE.text "Ultra fancy algorithms",
                                          HE.br,
                                          HE.text "make the chat matches.",
                                          HE.br,
                                          HE.text "Random, but in an interesting way"
                                    ]
                              ],
                              HE.div (HA.class' "point-column") [
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point3.png"],
                                          HE.br,
                                          HE.text "Feeling uninspired?",
                                          HE.br,
                                          HE.text "Let the app create your profile,",
                                          HE.br,
                                          HE.text "or suggest what to say"
                                    ],
                                    HE.div (HA.class' "point") [
                                          HE.img [HA.class' "point-melon", HA.src "/client/media/point6.png"],
                                          HE.br,
                                          HE.text "Send text, image & audio messages.",
                                          HE.br,
                                          HE.text "Choose to take part in ",
                                          HE.br,
                                          HE.text "novel chat experiments"
                                    ]
                              ]
                        ],
                        HE.div (HA.class' "second-point") [
                              HE.div (HA.class' "point") [
                                    HE.img [HA.class' "point-melon", HA.src "/client/media/point7.png"],
                                    HE.br,
                                    HE.text "Full of watermelons!"
                              ]
                        ],
                        HE.h2_ [
                              HE.text "This is how it works:"
                        ],
                        HE.div (HA.class' "third-points") [
                              HE.div (HA.class' "point") [
                                    HE.img [HA.class' "point-melon", HA.src "/client/media/works1.png"],
                                    HE.br,
                                    HE.text "New users randomized account needs Karma to access",
                                    HE.br,
                                    HE.text "features and tools. Karma is earned by making",
                                    HE.br,
                                    HE.text "great conversations, and being a good user.",
                                    HE.br,
                                    HE.br,
                                    HE.text "Creeps get weeded out; interesting folks get more visibility"
                              ],
                              HE.div (HA.class' "point") [
                                    HE.img [HA.class' "point-melon", HA.src "/client/media/works2.png"],
                                    HE.br,
                                    HE.text "Whenever you feel like chatting, the system matches",
                                    HE.br,
                                    HE.text "you with a random user- and vice versa,",
                                    HE.br,
                                    HE.text "ensuring no one gets ignored.",
                                    HE.br,
                                    HE.br,
                                    HE.text "You decide what to share and who to share it with"
                              ],
                              HE.div (HA.class' "point") [
                                    HE.img [HA.class' "point-melon", HA.src "/client/media/works3.png"],
                                    HE.br,
                                    HE.text "Want to take a break, or delete your account forever?",
                                    HE.br,
                                    HE.text "No worries, your personal data will never be used",
                                    HE.br,
                                    HE.text "for spam, harvesting or any shady dealings.",
                                    HE.br,
                                    HE.br,
                                    HE.text "Reactivate your account whenever you want"
                              ]
                        ],
                        HE.div (HA.class' "second-green-box") [
                              HE.div (HA.class' "point-2") [
                                    HE.text "MelanChat helps you talk, interact with people and form real friendships -- ",
                                    HE.br,
                                    HE.text "without fear of the dreaded ASL questions or getting stalked"
                              ],
                              HE.div (HA.class' "point-2") [
                                    HE.text "MelanChat is not affiliated with any Big Tech companies; nor does it track or spy on you.",
                                    HE.br,
                                    HE.text "Neither it is a dating website. Why upload duck-face pictures when you can talk about",
                                    HE.br,
                                    HE.text "dancing plagues of the 16th century?"
                              ],
                              HE.div (HA.class' "point-2") [
                                    HE.text "Lastly, did I mention that watermelons are everywhere?",
                                    HE.br,
                                    HE.text "It can't get better (or healthier) than this"
                              ],
                              HE.a [HA.class' "form-up-call", HA.href "#headline"] $ "... and it is free! Click here to create an account and chat right away #melanchat"
                        ],
                        externalFooter
                  ]
            ]
