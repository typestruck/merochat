module Server.Landing.Template where

import Prelude

import Effect (Effect)
import Environment (production)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.External.Svg as SES
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
            | production = [ HE.style [ HA.type' "text/css" ] "666 style.css 666" ] --used to inline stylesheets for production
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
                                    [ HE.strong_ "Feeling chatty? In search of new friends? Bored?"
                                    , HE.br
                                    , HE.strong_ "MeroChat"
                                    , HE.text " is the space to connect with people who"
                                    , HE.br
                                    , HE.text "are also looking for "
                                    , HE.strong_ "quality chats"
                                    , HE.br
                                    , HE.text " Text only, no groups or video, and"
                                    , HE.strong_ " strictly platonic"
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
                    , HE.div (HA.class' "features skew-left")
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
                    , HE.div (HA.class' "features skew-right")
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
                                            , HE.span_ "MeroChat is solely for human beings!"
                                            ]

                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Smart matching"
                                            , HE.span_ "Ultra-fancy algorithms pair you"
                                            , HE.br
                                            , HE.span_ "with interesting people!"
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
                                            , HE.span (HA.class' "subtagline-again") "The friendliest place to chat on the internet"
                                            ]
                                    ]
                            , HE.a [ HA.class' "try-it shadow", HA.href "#header" ] "Try it now!"
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
                                                    [ HE.p_
                                                            [ HE.text "Our formula is a little different. Whereas other chat apps are completly anonymous,"
                                                            , HE.br
                                                            , HE.text "video based, or geared towards instantaneousness, MeroChat offers you a more grounded experience"
                                                            ]
                                                    , HE.p_
                                                            [ HE.text "We want to encourage substantial (or at least fun!) conversations, and for that reason your profile"
                                                            , HE.br
                                                            , HE.text " on MeroChat can be personalized with as much info as you want. Conversation starters! A bit of personality."
                                                            , HE.br
                                                            , HE.text "Blank screens with a text box are no fun"
                                                            ]
                                                    , HE.p_
                                                            [ HE.text "While we can't exactly predict who you will click with, hopefully it is everyone."
                                                            , HE.br
                                                            , HE.text "Chat suggestions can be skipped, but not filtered by gender, age, location, etc. Text based communication"
                                                            , HE.br
                                                            , HE.text "is better for quality since it allows users to reply at their own time, without the possible nastiness of video calls"
                                                            ]
                                                    , HE.p_
                                                            [ HE.text "Speaking of which, MeroChat is for friendly conversation only. To ensure that, besides traditional moderation, "
                                                            , HE.br
                                                            , HE.text "we have a Karma system. The idea is pretty simple: the more the community trusts you, the more features you"
                                                            , HE.br
                                                            ,HE.text "can use on the app. The likelier a feature is to be abused (say, image sharing) the more Karma you need to unlock it"
                                                            ]
                                                , HE.p_ [HE.text "Finally, MeroChat is completly free! It exists for the fun of it, not to sell ads, train AI, or be a startup. Give it a try :)"]
                                                    ]
                                            ]
                                    ]
                            ]

                    ]
            ]
