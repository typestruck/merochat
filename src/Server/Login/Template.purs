module Server.Login.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

template ∷ Effect String
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , javascript = javascript
            , title = "MeroChat - Login"
            }
      FRS.render contents
      where
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Login Js ]
            ]
      content =
            [ HE.div (HA.class' "pastel-area column")

                    [ HE.div_ "Login to MeroChat!"
                    , HE.div (HA.class' "sign-up-form form-up")
                            [ HE.div [ HA.id $ show EmailDiv, HA.class' "input" ]
                                    [ HE.input [ HA.placeholder "Email", HA.type' "text", HA.maxlength emailMaxCharacters, HA.id $ show EmailInput ]
                                    , HE.span (HA.class' "error-message") "Please enter a valid email"
                                    ]
                            , HE.div [ HA.id $ show PasswordDiv, HA.class' "input" ]
                                    [ HE.input [ HA.placeholder "Password", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.id $ show PasswordInput ]
                                    , HE.span (HA.class' "error-message") $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                                    ]
                            , HE.div [ HA.class' "input" ]
                                    [ HE.input [ HA.type' "button", HA.value "Log in" ]
                                    , HE.span' [ HA.class' "request-error-message error-message" ]
                                    ]
                            , HE.div (HA.class' "forgot-sign-up")
                                    [ HE.a [ HA.href $ routes.recover.get { query: { token: Nothing } }, HA.class' "question-link forgot" ] "Forgot your password?"
                                    , HE.span [ HA.class' "or" ] "or"
                                    , HE.a [ HA.href $ routes.landing {}, HA.class' "question-link" ] "Don't have an account?"
                                    ]
                            ]
                    ]
            ]