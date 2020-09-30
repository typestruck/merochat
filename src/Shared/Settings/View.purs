module Shared.Settings.View where

import Shared.Types

import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

view :: SettingsModel -> Html SettingsMessage
view model =
      HE.div (HA.class' "settings-edition") [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/settings.css"],
            account model
      ]

account :: SettingsModel -> Html SettingsMessage
account { email, emailConfirmation, password, passwordConfirmation} =
      HE.div (HA.class' "settings-section") [
            HE.div (HA.class' "section-label") [
                  HE.div (HA.class' "bold") "Account",
                  HE.div (HA.class' "duller") [
                        HE.text "Change your password, email",
                        HE.br,
                        HE.text "or close your account"
                  ]
            ],
            HE.div_ [
                  HE.label_ "Email",
                  HE.input [HA.type' "text", HA.value email, HA.onInput SetEmail],

                  HE.label_ "Confirm email",
                  HE.input [HA.type' "text", HA.value emailConfirmation, HA.onInput SetEmailConfirmation],

                  HE.input [HA.type' "button", HA.class' "green-button", HA.value "Change email", HA.onClick ChangeEmail],
                  HE.br,

                  HE.label_ "Password",
                  HE.input [HA.type' "password", HA.onInput SetPassword],

                  HE.label_ "Confirm password",
                  HE.input [HA.type' "password", HA.onInput SetPasswordConfirmation],

                  HE.input [HA.type' "button", HA.class' "green-button", HA.value "Change password", HA.onClick ChangePassword],
                  HE.br,

                  HE.label_ "Permanently delete all my data and close my account",
                  HE.input [HA.type' "button", HA.value "Terminate account", HA.class' "green-button danger", HA.onClick TerminateAccount]
            ]
      ]

