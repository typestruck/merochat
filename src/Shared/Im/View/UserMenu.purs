module Shared.Im.View.UserMenu where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Im.Svg as SIS
import Shared.Intl as SI

userMenu ∷ ImModel → Html ImMessage
userMenu model =
      HE.div (HA.class' { settings: true, highlighted: model.toggleModal == Tutorial OptionsMenu })
            [ header model
            , HE.div [ HA.class' "outer-user-menu" ]
                    [ SIS.home
                    , HE.svg [ HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 50 50" ]
                            [ HE.path' [ HA.d "M47.16,21.221l-5.91-0.966c-0.346-1.186-0.819-2.326-1.411-3.405l3.45-4.917c0.279-0.397,0.231-0.938-0.112-1.282 l-3.889-3.887c-0.347-0.346-0.893-0.391-1.291-0.104l-4.843,3.481c-1.089-0.602-2.239-1.08-3.432-1.427l-1.031-5.886 C28.607,2.35,28.192,2,27.706,2h-5.5c-0.49,0-0.908,0.355-0.987,0.839l-0.956,5.854c-1.2,0.345-2.352,0.818-3.437,1.412l-4.83-3.45 c-0.399-0.285-0.942-0.239-1.289,0.106L6.82,10.648c-0.343,0.343-0.391,0.883-0.112,1.28l3.399,4.863 c-0.605,1.095-1.087,2.254-1.438,3.46l-5.831,0.971c-0.482,0.08-0.836,0.498-0.836,0.986v5.5c0,0.485,0.348,0.9,0.825,0.985 l5.831,1.034c0.349,1.203,0.831,2.362,1.438,3.46l-3.441,4.813c-0.284,0.397-0.239,0.942,0.106,1.289l3.888,3.891 c0.343,0.343,0.884,0.391,1.281,0.112l4.87-3.411c1.093,0.601,2.248,1.078,3.445,1.424l0.976,5.861C21.3,47.647,21.717,48,22.206,48 h5.5c0.485,0,0.9-0.348,0.984-0.825l1.045-5.89c1.199-0.353,2.348-0.833,3.43-1.435l4.905,3.441 c0.398,0.281,0.938,0.232,1.282-0.111l3.888-3.891c0.346-0.347,0.391-0.894,0.104-1.292l-3.498-4.857 c0.593-1.08,1.064-2.222,1.407-3.408l5.918-1.039c0.479-0.084,0.827-0.5,0.827-0.985v-5.5C47.999,21.718,47.644,21.3,47.16,21.221z M25,32c-3.866,0-7-3.134-7-7c0-3.866,3.134-7,7-7s7,3.134,7,7C32,28.866,28.866,32,25,32z" ] ]
                    --             , HE.div (HA.class' "mobile-profile-header") $ header model

                    ]
            , HE.span [ HA.class' "suggestions-button", HA.onClick $ ToggleInitialScreen false ] $
                    HE.svg [ HA.class' "svg-suggestion-button", HA.viewBox "0 0 16 16" ]
                          [ HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM4.42,10l-.06.07-.49.59,0,.05-.93,1.13V9.54h0V3.41a.68.68,0,0,1,.61-.73h8.14a.67.67,0,0,1,.59.73V8.86a.72.72,0,0,1-.64.78H4.72Zm9.18,3.33-.92-1-.52-.57-.41-.45H4.9l.76-1h6.15a1,1,0,0,0,1-.9V5.11H13a.63.63,0,0,1,.61.66v5.49h0Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M12.26,8.86V3.41a.67.67,0,0,0-.59-.73H3.53a.68.68,0,0,0-.61.73V9.54h0v2.31l.93-1.13,0-.05.49-.59L4.42,10l.3-.37h6.9A.72.72,0,0,0,12.26,8.86Zm-1-4.11A3.62,3.62,0,0,1,7.68,8.23,3.66,3.66,0,0,1,5.11,7.16a3.59,3.59,0,0,1-1.05-2.4V4.1h7.26Z" ]
                          , HE.path' [ HA.class' "strokeless", HA.d "M4.06,4.76a3.59,3.59,0,0,0,1.05,2.4A3.66,3.66,0,0,0,7.68,8.23,3.62,3.62,0,0,0,11.3,4.75l0-.65H4.06Zm5.55.48a.43.43,0,1,1-.43.43A.43.43,0,0,1,9.61,5.24Zm-1.93,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Zm-1.92,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M5.76,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M7.68,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M9.61,6.1a.43.43,0,1,0-.43-.43A.43.43,0,0,0,9.61,6.1Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M13.59,5.77A.63.63,0,0,0,13,5.11h-.2V9.38a1,1,0,0,1-1,.9H5.66l-.76,1h6.85l.41.45.52.57.92,1V11.26h0Z" ]
                          ]
            ]

header ∷ ImModel → Html ImMessage
header model = HE.fragment
      [ HE.img [ HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.title "Edit your profile", HA.class' "avatar-settings", HA.src $ SA.fromAvatar model.user ]
      , HE.div [ HA.class' "settings-name" ]
              [ HE.strong (HA.class' "contact-name") model.user.name
              , HE.div [ HA.class' "settings-karma", HA.onClick <<< SpecialRequest $ ToggleModal ShowKarmaPrivileges, HA.title "See your privileges and karma stats" ]
                      [ HE.span [ HA.class' "karma-number" ] $ SI.thousands model.user.karma
                      , HE.span (HA.class' "duller") $ " karma • (#" <> show model.user.karmaPosition <> ")"
                      ]
              ]
      ]