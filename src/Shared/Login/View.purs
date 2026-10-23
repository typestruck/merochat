module Shared.Login.View where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Element (ElementId(..))
import Shared.Change as SC
import Shared.Extra as SE
import Shared.Login.Types (LoginModel, LoginMessage(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Routes (routesSpec)

view ∷ LoginModel → Html LoginMessage
view model = HE.div_
      [ HE.div [ HA.class' "pastel-area" ]
              [ HE.div [ HA.class' "sign-up-form form-up" ]
                      [ HE.div [ HA.class' "input" ]
                              [ HE.input [ HA.placeholder "Email", HA.createAttribute "placeholder-color" "#808080", HA.type' "text", HA.maxlength emailMaxCharacters, HA.id $ show EmailInput, HA.value $ DM.fromMaybe "" model.email, SC.onChange $ SetEmail <<< SC.toMaybe, HA.disabled model.loading, HA.onConfirm $ const $ KeyPressed "Enter" ]
                              , HE.span [ HA.class' "error-message" ] [ HE.text "Please enter a valid email" ]
                              ]
                      , HE.div [ HA.class' "input" ]
                              [ HE.input [ HA.placeholder "Password", HA.createAttribute "placeholder-color" "#808080", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.id $ show PasswordInput, HA.value $ DM.fromMaybe "" model.password, SC.onChange $ SetPassword <<< SC.toMaybe, HA.disabled model.loading, HA.onConfirm $ const $ KeyPressed "Enter" ]
                              , HE.span [ HA.class' "error-message" ] [ HE.text $ "Password must be " <> show passwordMinCharacters <> " characters or more" ]
                              ]
                      , HE.div [ HA.class' "input" ]
                              [ HE.button [ HA.type' "button", HA.class' "login-button", HA.onClick Submit, HA.disabled model.loading ] [ HE.text $ if model.loading then "Logging in…" else "Log in" ]
                              , HE.span [ HA.class' "login-error" ] [ HE.text $ DM.fromMaybe "" model.error ]
                              ]
                      , HE.div [ HA.class' "forgot-sign-up lynx-inline" ]
                              [ SE.a [ HA.href $ routesSpec.recover.get { query: { token: Nothing } }, HA.class' "question-link forgot" ] [ HE.text "Forgot your password?" ]
                              , HE.span [ HA.class' "or" ] [ HE.text "or" ]
                              , SE.a [ HA.href $ routesSpec.landing {}, HA.class' "question-link" ] [ HE.text "Don't have an account?" ]
                              ]
                      ]
              ]
      ]
