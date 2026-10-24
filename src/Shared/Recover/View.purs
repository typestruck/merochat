module Shared.Recover.View where

import Prelude

import Data.Maybe as DM
import Effect (Effect)
import Flame (Html)
import Flame.Types (NodeData)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Change as SC
import Shared.Element (ElementId(..))
import Shared.External.Types (Page(..))
import Shared.Extra as SE
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Recover.Types (RecoverMessage(..), RecoverModel)
import Shared.Routes (routesSpec)

view ∷ RecoverModel → Html RecoverMessage
view = viewWithEvents $ const []

viewWithNavigation ∷ (Page → Effect Unit) → RecoverModel → Html RecoverMessage
viewWithNavigation = viewWithEvents <<< (<<<) (pure <<< SE.onClick)

viewWithEvents ∷ (Page → Array (NodeData RecoverMessage)) → RecoverModel → Html RecoverMessage
viewWithEvents events model = HE.div [ HA.class' "pastel-area column" ]
      [ HE.div [ HA.class' "sign-up-form form-up" ] $
              [ SE.h2_ [ HE.text $ if DM.isJust model.token then "Reset your password" else "Recover your account" ]
              ] <>
              ( if DM.isJust model.token then
                      [ HE.div [ HA.class' "input" ]
                              [ HE.input [ HA.id $ show PasswordInput, HA.placeholder "Password", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.value $ DM.fromMaybe "" model.password, HA.disabled model.loading, SC.onChange $ SetPassword <<< SC.toMaybe, HA.onConfirm $ const Submit ]
                              , HE.span [ HA.class' "error-message" ] [ HE.text $ "Password must be " <> show passwordMinCharacters <> " characters or more" ]
                              ]
                      , HE.div [ HA.class' "input" ]
                              [ HE.input [ HA.id $ show PasswordConfirmationInput, HA.placeholder "Confirm password", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.value $ DM.fromMaybe "" model.confirmation, HA.disabled model.loading, SC.onChange $ SetConfirmation <<< SC.toMaybe, HA.onConfirm $ const Submit ]
                              , HE.span [ HA.class' "error-message" ] [ HE.text "Password and confirmation do not match" ]
                              ]
                      ]
                else
                      [ HE.div [ HA.class' "input" ]
                              [ HE.input [ HA.id $ show EmailInput, HA.placeholder "Email", HA.type' "text", HA.maxlength emailMaxCharacters, HA.value $ DM.fromMaybe "" model.email, HA.disabled model.loading, SC.onChange $ SetEmail <<< SC.toMaybe, HA.onConfirm $ const Submit ]
                              , HE.span [ HA.class' "error-message" ] [ HE.text "Please enter a valid email" ]
                              ]
                      ]
              ) <>
              [ HE.button [ HA.type' "button", HA.class' "login-button", HA.onClick Submit, HA.disabled model.loading, HA.style { pointerEvents: if model.loading then "none" else "auto" } ] [ HE.text $ if model.loading then "Please wait…" else if DM.isJust model.token then "Change password" else "Send recovery email" ]
              , HE.div [ HA.class' "login-error request-error-message" ] [ HE.text $ DM.fromMaybe "" model.error ]
              , HE.div [ HA.id "routes-success-message", HA.class' $ if DM.isJust model.success then "public-success" else "success-message" ]
                      [ HE.text $ DM.fromMaybe (if DM.isJust model.token then "Password changed. Redirecting to login…" else "Recovery email sent. Please check your inbox.") model.success ]
              , HE.div [ HA.class' "forgot-sign-up" ]
                      [ SE.a ([ HA.href $ routesSpec.landing {}, HA.class' "question-link" ] <> events Landing) [ HE.text "Don't have an account?" ] ]
              ]
      ]
