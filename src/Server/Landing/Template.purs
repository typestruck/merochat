module Server.Landing.Template where

import Prelude

import Effect (Effect)
import Environment (production)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.External.Svg as SES
import Shared.Element (ElementId(..))
import Shared.Options.Profile (passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP

template ∷ Effect String
template = do
      contents ← ST.template externalDefaultParameters
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
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Landing Css ]
                    ]
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Landing Js ]
            , HE.script' [ HA.createAttribute "async" "true", HA.src "https://www.google.com/recaptcha/api.js?onload=initCaptchas&render=explicit" ]
            ]
      content =
            [ HE.div (HA.class' "landing")
                    [ HE.div [ HA.class' "pastel-area" ]
                            [ HE.div (HA.class' "blurb")
                                    [ HE.text "Feeling chatty? In search of new friends? Bored?"
                                    , HE.br
                                    , HE.text "MeroChat connects you with random people who"
                                    , HE.br
                                    , HE.text "are here only for chatting. Text only, no groups"
                                    , HE.br
                                    , HE.text "or video chat!"
                                    ]
                            , HE.div (HA.class' "sign-up-form form-up")
                                    [ HE.div' [ HA.id $ show CaptchaRegularUser, HA.class' "hidden" ]
                                    , HE.div (HA.id $ show EmailDiv)
                                            [ HE.input [ HA.id $ show EmailInput, HA.type' "text", HA.autocomplete "new-email", HA.placeholder "Email" ]
                                            , HE.div (HA.class' "error-message") "Please enter a valid email"
                                            ]
                                    , HE.div (HA.id $ show PasswordDiv)
                                            [ HE.input [ HA.id $ show PasswordInput, HA.type' "password", HA.maxlength passwordMaxCharacters, HA.autocomplete "new-password", HA.placeholder "Password" ]
                                            , HE.div (HA.class' "error-message") $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                                            ]
                                    , HE.div' [ HA.id $ show CaptchaTemporaryUser, HA.class' "hidden" ]
                                    , HE.input [ HA.class' "shadow", HA.type' "button", HA.value "Create account" ]
                                    , HE.a (HA.id $ show TemporaryUserSignUp) "Or continue as a guest →"
                                    , HE.span' [ HA.class' "request-error-message error-message" ]
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
                                            [ SES.feature1
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Privacy first"
                                            , HE.text "No ads or data harvesting."
                                            , HE.br
                                            , HE.text "Take a break or delete"
                                            , HE.br
                                            , HE.text "your account any time!"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SES.feature2
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SES.feature3
                                            ]
                                    , HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Anonymously you"
                                            , HE.span_ "Share as much (or as little)"
                                            , HE.br
                                            , HE.text "as you want. Your data belongs to you!"
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SES.feature4
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
                                            [ SES.feature5
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
                                            [ SES.feature6
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "sign-up-again")
                            [ HE.div (HA.class' "logo-name")
                                    [ SES.invertedLogo
                                    , HE.div (HA.class' "tagline-name")
                                            [ HE.h1 (HA.class' "name-again") "MeroChat"
                                            , HE.span (HA.class' "subtagline-again") "Friendly Random Chat"
                                            ]
                                    ]
                            , HE.a [ HA.class' "try-it shadow", HA.href "#header" ] "Try it out"
                            ]
                    , HE.div (HA.class' "features-again")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-right")
                                            [ SES.feature7
                                            ]
                                    , HE.div (HA.class' "feature-blurb-left")
                                            [ HE.span (HA.class' "green-call") "MeroChat lets you chat, connect and make real friends "
                                            , HE.br
                                            , HE.span (HA.class' "green-call") "— without the dreaded “ASL” questions or creeps"
                                            , HE.div (HA.class' "after-green-call")
                                                    [ HE.text "Free software, no ties to Big Tech, and we never track, spy or use your personal data."
                                                    , HE.br
                                                    , HE.text " Also, not a dating site! Why upload duck-face selfies when you could talk about dancing plagues"
                                                    , HE.br
                                                    , HE.text "of the 16th century?"
                                                    ]
                                            ]
                                    ]
                            ]

                    ]
            ]
