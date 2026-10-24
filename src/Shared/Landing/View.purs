module Shared.Landing.View where

import Prelude

import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Change as SC
import Shared.Element (ElementId(..))
import Shared.Extra as SX
import Shared.Landing.Types (LandingMessage(..), LandingModel)
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)

view ∷ LandingModel → Html LandingMessage
view model = HE.div [ HA.class' "sign-up-form form-up" ]
      [ SX.h2_ [ HE.text "Create an account" ]
      , HE.div_
              [ HE.input [ HA.id $ show EmailInput, HA.autocomplete "new-email", HA.placeholder "Email", HA.createAttribute "placeholder-color" "#808080", HA.type' "text", HA.maxlength emailMaxCharacters, HA.value $ DM.fromMaybe "" model.email, HA.disabled model.loading, SC.onChange $ SetEmail <<< SC.toMaybe, HA.onConfirm $ const Submit ]
              , HE.div [ HA.class' "error-message" ] [ HE.text "Please enter a valid email" ]
              ]
      , HE.div_
              [ HE.input [ HA.id $ show PasswordInput, HA.autocomplete "new-password", HA.placeholder "Password", HA.createAttribute "placeholder-color" "#808080", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.value $ DM.fromMaybe "" model.password, HA.disabled model.loading, SC.onChange $ SetPassword <<< SC.toMaybe, HA.onConfirm $ const Submit ]
              , HE.div [ HA.class' "error-message" ] [ HE.text $ "Password must be " <> show passwordMinCharacters <> " characters or more" ]
              ]
      , HE.button [ HA.id $ show CreateAccount, HA.type' "button", HA.class' "login-button", HA.onClick Submit, HA.disabled model.loading, HA.style { pointerEvents: if model.loading then "none" else "auto" } ] [ HE.text $ if model.loading then "Please wait…" else "Create account" ]
      , HE.div [ HA.class' "login-error request-error-message" ] [ HE.text $ DM.fromMaybe "" model.error ]
      --pointerevents is what lynx uses to disable views
      , HE.button [ HA.id $ show TemporaryUserSignUp, HA.type' "button", HA.class' "public-guest", HA.onClick Guest, HA.disabled model.loading, HA.style { pointerEvents: if model.loading then "none" else "auto" } ] [ HE.text "Or continue as a guest →" ]
      ]
