module Shared.Im.View.UserMenu where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Element (ElementId(..))
import Shared.Im.Svg as SIS
import Shared.Intl as SI
import Shared.Svg as SS

userMenu ∷ ImModel → Html ImMessage
userMenu model =
      HE.div (HA.class' { settings: true, highlighted: model.toggleModal == Tutorial OptionsMenu })
            [ header model
            , HE.div [ HA.class' "outer-user-menu" ]
                    [ SIS.home
                    , HE.svg [ HA.id $ show UserContextMenu, HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 24 24" ]
                            [ HE.path' [ HA.d "M19.6515 19.4054C20.2043 19.2902 20.5336 18.7117 20.2589 18.2183C19.6533 17.1307 18.6993 16.1749 17.4788 15.4465C15.907 14.5085 13.9812 14 12 14C10.0188 14 8.09292 14.5085 6.52112 15.4465C5.30069 16.1749 4.34666 17.1307 3.74108 18.2183C3.46638 18.7117 3.79562 19.2902 4.34843 19.4054C9.39524 20.4572 14.6047 20.4572 19.6515 19.4054Z" ]
                            , HE.circle' [ HA.cx "12", HA.cy "8", HA.r "5" ]
                            ]
                    , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowUserContextMenu } ]
                            [ HE.div (HA.class' "user-menu-item")
                                    [ SS.sun
                                    , SS.moon
                                    ]
                            , HE.div (HA.class' "mobile-profile-header") $ header model
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowProfile
                                    , HE.span_ "Set your profile picture, name"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowSettings
                                    , HE.span_ "Change email, password, etc"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowKarmaPrivileges ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowKarmaPrivileges
                                    , HE.span_ "See your privileges, karma stats"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowExperiments
                                    , HE.span_ "Talk in novel ways"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowBacker ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowBacker
                                    , HE.span_ "Donate or become a patron"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp ]
                                    [ HE.div (HA.class' "menu-item-heading") "Help"
                                    , HE.span_ "Learn more about MeroChat"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowFeedback ]
                                    [ HE.div (HA.class' "menu-item-heading") "Send feedback"
                                    , HE.span_ "Report issues, or leave a question"
                                    ]
                            , if model.user.temporary then
                                    HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmTerminationTemporaryUser ] "Delete my data"
                              else
                                    HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmLogout ] "Logout"

                            ]
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