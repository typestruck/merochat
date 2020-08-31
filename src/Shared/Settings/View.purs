module Shared.Settings.View where

import Prelude


import Shared.Types

import Data.Array ((:), (..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.DateTime as SDT
import Shared.Unsafe as SU

view :: SettingsModel -> Html SettingsMessage
view model =
        HE.div (HA.class' "settings-edition") [
                account model
        ]

account :: SettingsModel -> Html SettingsMessage
account ({ email, emailConfirmation, password, passwordConfirmation}) =
        HE.div_ [
                HE.fieldset_ [
                        HE.legend_ "Account",
                        HE.div_ [
                                HE.label_ "Email",
                                HE.input [HA.type' "text", HA.value email, HA.onInput SetEmail]
                        ],
                        HE.div_ [
                                HE.label_ "Confirm email",
                                HE.input [HA.type' "text", HA.value emailConfirmation, HA.onInput SetEmailConfirmation]
                        ],
                        HE.input [HA.type' "button", HA.value "Change email", HA.class' "action-button", HA.onClick ChangeEmail],
                        HE.br,
                        HE.div_ [
                                HE.label_ "Password",
                                HE.input [HA.type' "password", HA.onInput SetPassword]
                        ],
                        HE.div_ [
                                HE.label_ "Confirm password",
                                HE.input [HA.type' "password", HA.onInput SetPasswordConfirmation]
                        ],
                        HE.input [HA.type' "button", HA.value "Change password", HA.class' "action-button", HA.onClick ChangePassword],
                        HE.br,
                        HE.br,
                        HE.div_ [
                                HE.label_ "Permanently delete all data and close my account",
                                HE.br,
                                HE.input [HA.type' "button", HA.value "Terminate account", HA.class' "action-button", HA.onClick TerminateAccount]
                        ]
                ]
        ]

