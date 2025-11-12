module Shared.Experiments.PaperPlane where

import Prelude

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (ExperimentsMessage(..), ExperimentsModel)
import Shared.Options.PaperPlane (maxMessageCharacters, maxPaperPlanes)

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "paper-plane duller" ]
      [ HE.div [ HA.class' "green-tab" ]
              [ HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": true } ] [ HE.text "New" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": false } ] [ HE.text "Flying by" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": false } ] [ HE.text "Caught" ]
              ]
      , case model.section of
              _ → new model
      ]

new ∷ ExperimentsModel → Html ExperimentsMessage
new model = HE.div [ HA.class' "new-throw" ]
      [ HE.textarea'
              [ HA.maxlength maxMessageCharacters
              , HA.onInput' ResizeMessageInput
              , HA.autocomplete "off"
              , HA.class' "modal-input paper-input "
              , HA.value $ DM.fromMaybe "" model.paperPlane.message
              , HA.onInput SetPlaneMessage
              , HA.placeholder "What does the world need to know?"
              ]
      , if DA.length model.paperPlane.thrown == maxPaperPlanes then
              HE.div [ HA.class' "paper-waiting" ] [ HE.text "Waiting for planes to be caught..." ]
        else if model.paperPlane.loading then
              HE.div' [ HA.class' "loading" ]
        else
              HE.input
                    [ HA.type' "button"
                    , HA.onClick ThrowPlane
                    , HA.disabled $ DM.isNothing model.paperPlane.message
                    , HA.class' "green-button"
                    , HA.value "Throw paper plane"
                    ]
      , HE.fragment $
              if DA.null model.paperPlane.thrown then
                    []
              else
                    (planeIcon : map plane model.paperPlane.thrown)
      ]
      where
      plane thrown = HE.div [ HA.class' "paper-thrown-entry" ]
            [ HE.div [ HA.class' "paper-thrown-message" ] [ HE.text thrown.message ]
            , HE.div [] [ HE.text $ show thrown.status ]
            ]

      planeIcon = HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 256 256" ]
            [ HE.g [ HA.styleAttr "stroke-linecap: butt; stroke-linejoin: miter; stroke-miterlimit: 10; fill-rule: nonzero; opacity: 1;", HA.transform "translate(1.4065934065934016 1.4065934065934016) scale(2.81 2.81)" ]
                    [ HE.path' [ HA.d "M 89.981 6.2 C 90 6.057 90.001 5.915 89.979 5.775 c -0.003 -0.021 -0.001 -0.041 -0.005 -0.062 c -0.033 -0.163 -0.098 -0.317 -0.183 -0.462 c -0.009 -0.016 -0.01 -0.033 -0.019 -0.049 c -0.015 -0.024 -0.039 -0.036 -0.055 -0.059 c -0.034 -0.048 -0.06 -0.102 -0.101 -0.146 c -0.051 -0.056 -0.113 -0.097 -0.17 -0.144 c -0.031 -0.025 -0.058 -0.054 -0.09 -0.076 c -0.134 -0.093 -0.28 -0.164 -0.436 -0.209 c -0.028 -0.008 -0.056 -0.009 -0.084 -0.015 c -0.132 -0.03 -0.267 -0.041 -0.404 -0.034 c -0.046 0.002 -0.089 0.006 -0.135 0.012 c -0.039 0.006 -0.079 0.002 -0.118 0.01 l -87 19.456 c -0.611 0.137 -1.073 0.639 -1.159 1.259 c -0.085 0.62 0.224 1.229 0.775 1.525 l 23.523 12.661 l 7.327 23.36 c 0.008 0.025 0.025 0.043 0.034 0.067 c 0.021 0.056 0.052 0.106 0.08 0.16 c 0.059 0.114 0.127 0.218 0.211 0.312 c 0.022 0.025 0.03 0.057 0.054 0.08 c 0.022 0.021 0.05 0.028 0.073 0.048 c 0.099 0.086 0.207 0.155 0.325 0.213 c 0.047 0.023 0.088 0.053 0.136 0.07 c 0.164 0.061 0.336 0.1 0.517 0.1 c 0.011 0 0.022 0 0.033 0 c 0.179 -0.004 0.349 -0.044 0.509 -0.107 c 0.041 -0.016 0.075 -0.044 0.114 -0.063 c 0.127 -0.063 0.244 -0.139 0.349 -0.235 c 0.02 -0.018 0.046 -0.024 0.065 -0.044 l 12.009 -12.209 l 23.18 12.477 c 0.221 0.119 0.466 0.18 0.711 0.18 c 0.188 0 0.378 -0.035 0.557 -0.107 c 0.412 -0.164 0.73 -0.504 0.869 -0.926 L 89.93 6.473 c 0.014 -0.044 0.015 -0.09 0.025 -0.135 C 89.966 6.292 89.975 6.247 89.981 6.2 z M 77.435 10.018 L 25.58 36.717 L 5.758 26.047 L 77.435 10.018 z M 74.32 14.997 L 36.813 43.768 c -0.003 0.002 -0.005 0.006 -0.007 0.008 c -0.112 0.087 -0.209 0.194 -0.294 0.314 c -0.018 0.025 -0.035 0.05 -0.051 0.076 c -0.017 0.028 -0.039 0.052 -0.055 0.081 c -0.054 0.1 -0.093 0.204 -0.122 0.309 c -0.001 0.005 -0.005 0.009 -0.006 0.014 L 32.96 56.977 l -5.586 -17.809 L 74.32 14.997 z M 35.992 57.249 l 2.693 -10.072 l 4.717 2.539 L 35.992 57.249 z M 69.177 60.184 L 40.479 44.737 l 45.09 -34.588 L 69.177 60.184 z", HA.styleAttr "stroke: none; stroke-width: 1; stroke-dasharray: none; stroke-linecap: butt; stroke-linejoin: miter; stroke-miterlimit: 10; fill-rule: nonzero; opacity: 1;", HA.transform " matrix(1 0 0 1 0 0) ", HA.strokeLinecap "round" ]
                    , HE.path' [ HA.d "M 12.9 85.482 c -0.38 0 -0.76 -0.144 -1.052 -0.431 c -0.591 -0.581 -0.599 -1.53 -0.018 -2.121 l 14.292 -14.528 c 0.581 -0.592 1.531 -0.598 2.121 -0.018 c 0.591 0.581 0.599 1.53 0.018 2.121 L 13.97 85.034 C 13.676 85.333 13.288 85.482 12.9 85.482 z", HA.styleAttr "stroke: none; stroke-width: 1; stroke-dasharray: none; stroke-linecap: butt; stroke-linejoin: miter; stroke-miterlimit: 10; fill-rule: nonzero; opacity: 1;", HA.transform " matrix(1 0 0 1 0 0) ", HA.strokeLinecap "round" ]
                    , HE.path' [ HA.d "M 36.431 79.593 c -0.38 0 -0.76 -0.144 -1.052 -0.431 c -0.591 -0.581 -0.599 -1.53 -0.018 -2.121 l 14.291 -14.527 c 0.582 -0.591 1.531 -0.598 2.121 -0.018 c 0.591 0.581 0.599 1.53 0.018 2.121 L 37.501 79.145 C 37.207 79.443 36.819 79.593 36.431 79.593 z", HA.styleAttr "stroke: none; stroke-width: 1; stroke-dasharray: none; stroke-linecap: butt; stroke-linejoin: miter; stroke-miterlimit: 10; fill-rule: nonzero; opacity: 1;", HA.transform " matrix(1 0 0 1 0 0) ", HA.strokeLinecap "round" ]
                    , HE.path' [ HA.d "M 8.435 67.229 c -0.38 0 -0.76 -0.144 -1.052 -0.431 c -0.591 -0.581 -0.599 -1.53 -0.018 -2.121 l 10.445 -10.618 c 0.581 -0.591 1.531 -0.598 2.121 -0.018 c 0.591 0.581 0.599 1.53 0.018 2.121 L 9.505 66.78 C 9.211 67.079 8.823 67.229 8.435 67.229 z", HA.styleAttr "stroke: none; stroke-width: 1; stroke-dasharray: none; stroke-linecap: butt; stroke-linejoin: miter; stroke-miterlimit: 10; fill-rule: nonzero; opacity: 1;", HA.transform " matrix(1 0 0 1 0 0) ", HA.strokeLinecap "round" ]
                    ]
            ]