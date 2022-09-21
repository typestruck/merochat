module Shared.Feedback.View where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

view ∷ _ → Html Unit
view {} =
      HE.div_
            [ HE.div (HA.class' "duller center")
                    [ HE.span_ "Use the form bellow to report any issues,"
                    , HE.br
                    , HE.span_ "send suggestions or shoot any questions you might have"
                    ]
            , HE.div_
                    [ HE.label_ "What would you like to say?"
                    , HE.textarea' [ HA.class' "comments modal-input" ]
                    , HE.div' (HA.class' "error-message")
                    ]
            , HE.div_
                    [ HE.label_ "Optionally, upload a screenshot"
                    , HE.input [ HA.type' "file", HA.class' "modal-input" ]
                    , HE.div' (HA.class' "error-message")
                    ]
            , HE.input [ HA.type' "button", HA.class' "green-button", HA.value "Send" ]
            , HE.div (HA.class' "duller center reach-out")
                    [ HE.br
                    , HE.span_ "You can also reach out at "
                    , HE.a [ HA.href "https://reddit.com/r/MeroChat", HA.target "_blank" ] "r/MeroChat"
                    , HE.span_ " and "
                    , HE.a [ HA.href "https://twitter.com/MeroChat", HA.target "_blank" ] "@MeroChat"
                    ]

            ]
