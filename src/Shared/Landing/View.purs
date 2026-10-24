module Shared.Landing.View where

import Prelude

import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Change as SC
import Shared.Element (ElementId(..))
import Shared.Landing.Types (LandingMessage(..), LandingModel)
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)

view ∷ LandingModel → Html LandingMessage
view model = HE.div [ HA.class' "sign-up-form form-up" ]
      [ HE.div_
              [ HE.input [ HA.id $ show EmailInput, HA.autocomplete "new-email", HA.placeholder "Email", HA.type' "text", HA.maxlength emailMaxCharacters, HA.value $ DM.fromMaybe "" model.email, HA.disabled model.loading, SC.onChange $ SetEmail <<< SC.toMaybe, HA.onConfirm $ const Submit ]
              , HE.div [ HA.class' "error-message" ] [ HE.text "Please enter a valid email" ]
              ]
      , HE.div_
              [ HE.input [ HA.id $ show PasswordInput, HA.autocomplete "new-password", HA.placeholder "Password", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.value $ DM.fromMaybe "" model.password, HA.disabled model.loading, SC.onChange $ SetPassword <<< SC.toMaybe, HA.onConfirm $ const Submit ]
              , HE.div [ HA.class' "error-message" ] [ HE.text $ "Password must be " <> show passwordMinCharacters <> " characters or more" ]
              ]
      , HE.div [ HA.class' "input" ]
              [ HE.button [ HA.id $ show CreateAccount, HA.type' "button", HA.class' { "login-button": true, "public-disabled": model.loading }, HA.onClick Submit, HA.disabled model.loading ] [ HE.text $ if model.loading then "Please wait…" else "Create account" ]
              ]
      , HE.div [ HA.class' { "login-error": true, "request-error-message": true, hidden: DM.isNothing model.error } ] [ HE.text $ DM.fromMaybe "" model.error ]
      , HE.button [ HA.id $ show TemporaryUserSignUp, HA.type' "button", HA.class' { "public-guest": true, "public-disabled": model.loading }, HA.onClick Guest, HA.disabled model.loading ] [ HE.text "Or continue as a guest →" ]
      ]
