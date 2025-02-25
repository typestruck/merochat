module Server.Landing.Template where

import Prelude

import Data.Either (Either(..))
import Data.String as DS
import Effect (Effect)
import Environment (production)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters, externalFooter)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Landing.Svg as SLS
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

template ∷ Effect String
template = do
      contents ← ST.template defaultParameters
            { content = content
            , javascript = javascript
            , css = css
            , bundled = production
            }
      FRS.render contents
      where
      css
            | production = [ HE.style [ HA.type' "text/css" ] "<% style.css %>" ] --used to inline stylesheets for production
            | otherwise =
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath External Css ]
                    , HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Landing Css ]
                    ]
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Landing Js ]
            , HE.script' [ HA.createAttribute "async" "true", HA.src "https://www.google.com/recaptcha/api.js?onload=initCaptchas&render=explicit" ]
            ]
      content =
            [ HE.div (HA.class' "landing")
                    [ HE.div (HA.class' "header")
                            [ HE.div (HA.class' "big-logo")
                                    [ SLS.logo

                                    ]
                            , HE.div (HA.class' "menu-merochat")
                                    [ HE.div (HA.class' "menu")
                                            [ HE.a_ "Home"
                                            , HE.a_ "FAQ"
                                            , HE.a_ "Support us"
                                            , HE.a [ HA.class' "login-link" ] "Login"
                                            , HE.div (HA.class' "theme-switcher")
                                                    [ SLS.sun
                                                    , SLS.moon
                                                    ]
                                            ]
                                    , HE.div (HA.class' "merochat")
                                            [ HE.h1 (HA.class' "name") "MeroChat"
                                            , HE.div (HA.class' "tagline") "Random chat without the sleaze"
                                            , HE.div (HA.class' "subtagline") "(Friendly only!)"
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "sign-up")
                            [ HE.div (HA.class' "blurb")
                                    [ HE.text "Feeling chatty? In search of new friends? Bored?"
                                    , HE.br
                                    , HE.text "MeroChat connects you with random people who"
                                    , HE.br
                                    , HE.text "are here only for chatting. Text only, no group"
                                    , HE.br
                                    , HE.text "or video chat!"
                                    ]
                            , HE.div (HA.class' "sign-up-form")
                                    [ HE.input [ HA.type' "text", HA.placeholder "Email" ]
                                    , HE.input [ HA.type' "text", HA.placeholder "Password" ]
                                    , HE.input [ HA.class' "shadow", HA.type' "button", HA.value "Create account" ]
                                    , HE.a_ "Continue as guest →"
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Safe and friendly"
                                            , HE.text "Strict moderation and a"
                                            , HE.br
                                            , HE.text "Karma system = friendly chats."
                                            , HE.br
                                            , HE.text "MeroChat is not a dating app!"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature1
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Privacy first"
                                            , HE.text "No ads or data harvesting."
                                            , HE.br
                                            , HE.text "Take a break or delete"
                                            , HE.br
                                            , HE.text "your account anytime!"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature2
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature3
                                            ]
                                    , HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Anonymously you"
                                            , HE.span_ "Share as much (or as little)"
                                            , HE.br
                                            , HE.text "as you want. Your data beloings to you!"
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature4
                                            ]
                                    , HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Human only"
                                            , HE.span_ "No bots, AI or spammers."
                                            , HE.br
                                            , HE.span_ "MeroChat is stricly for human beings!"
                                            ]

                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Smart matching"
                                            , HE.span_ "Ultra-fancy algorithms pair you with"
                                            , HE.br
                                            , HE.span_ "interesting people!"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature5
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Privileged features"
                                            , HE.span_ "Images, audio, and experimental"
                                            , HE.br
                                            , HE.span_ "features for trusted users!"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature6
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "sign-up-again")
                            [ HE.div (HA.class' "logo-name")
                                    [ SLS.invertedLogo
                                    , HE.div (HA.class' "tagline-name")
                                            [ HE.h1 (HA.class' "name-again") "MeroChat"
                                            , HE.span (HA.class' "subtagline-again") "Friendly Random Chat"
                                            ]
                                    ]
                            , HE.a (HA.class' "try-it shadow") "Try it out"
                            ]
                    , HE.div (HA.class' "features-again")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature7
                                            ]
                                    , HE.div (HA.class' "feature-blurb-left")
                                            [ HE.span (HA.class' "green-call") "MeroChat lets you chat, connect and make real friends "
                                            , HE.br
                                            , HE.span (HA.class' "green-call") "— without the dreaded “ASL” questions or creeps"
                                            , HE.div (HA.class' "after-green-call")
                                                    [ HE.text "Free software, no ties to Big Tech, and we never track, spy or use your personal data."
                                                    , HE.br
                                                    , HE.text " Also, not a dating site! Why upload duck-face selfies when you could talk about dancing plagues"
                                                    ,HE.br
                                                    , HE.text "of the 16th century?"
                                                    ]
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "footer")
                            [ HE.a_ "Terms and conditions"
                            , HE.a_ "Privacy policy"
                            , HE.a_ "Source code"
                            , HE.a_ "Support us"
                            ]
                    ]
            ]
