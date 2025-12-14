module Server.Recover.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Html (Html(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routesSpec)

template ∷ Maybe String → Effect Html
template token = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , javascript = javascript
            }
      Html <$> FRS.render contents
      where
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Recover Js ]
            ]
      content =
            [ HE.div [ HA.class' "pastel-area column" ]
                    [ HE.div [ HA.class' "sign-up-form form-up" ]
                            [ case token of
                                    Nothing →
                                          HE.fragment
                                                [ HE.div [ HA.class' "input" ]
                                                        [ HE.input [ HA.placeholder "Email", HA.type' "text", HA.id "email", HA.maxlength emailMaxCharacters, HA.id $ show EmailInput ]
                                                        , HE.span [ HA.class' "error-message" ] [ HE.text "Please enter a valid email" ]
                                                        ]
                                                , HE.div [ HA.class' "input" ]
                                                        [ HE.input [ HA.type' "button", HA.value "Recover" ]
                                                        ]
                                                , HE.span' [ HA.class' "routes-error-message error-message" ]
                                                , HE.span [ HA.id "routes-success-message", HA.class' "success-message" ] [ HE.text "Recovery email sent. Please check your inbox" ]
                                                , HE.div' [ HA.class' "g-recaptcha", HA.createAttribute "data-sitekey" "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", HA.id "captcha", HA.createAttribute "data-callback" "completeRecover", HA.createAttribute "data-size" "invisible" ]

                                                ]
                                    _ →
                                          HE.fragment
                                                [ HE.div [ HA.class' "input" ]
                                                        [ HE.input [ HA.type' "password", HA.placeholder "Password", HA.maxlength passwordMaxCharacters, HA.id $ show PasswordInput ]
                                                        , HE.span [ HA.class' "error-message" ] [ HE.text $ "Password must be " <> show passwordMinCharacters <> " characters or more" ]
                                                        ]
                                                , HE.div [ HA.class' "input" ]
                                                        [ HE.input [ HA.type' "password", HA.placeholder "Confirm password", HA.maxlength passwordMaxCharacters, HA.id $ show PasswordConfirmationInput ]
                                                        , HE.span [ HA.class' "error-message" ] [ HE.text "Password and confirmation do not match" ]
                                                        ]
                                                , HE.div [ HA.class' "input" ]
                                                        [ HE.input [ HA.type' "button", HA.value "Change password", HA.class' "action-button" ]
                                                        ]
                                                , HE.span' [ HA.class' "routes-error-message error-message" ]
                                                , HE.span [ HA.class' "success-message" ] [ HE.text "Password reseted. Redirecting to login..." ]

                                                ]
                            , HE.div [ HA.class' "forgot-sign-up" ]
                                    [ HE.a [ HA.href $ routesSpec.landing {}, HA.class' "question-link" ] [ HE.text "Don't have an account?" ]

                                    ]
                            ]
                    ]
            ]