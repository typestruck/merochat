module Shared.Im.View.UserMenu where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Shared.Intl as SI
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Element (ElementId(..))
import Shared.Im.Svg as SIS

userMenu ∷ ImModel → Html ImMessage
userMenu model@{ toggleContextMenu, toggleModal, user: { temporary } } =
      HE.div (HA.class' { settings: true, highlighted: toggleModal == Tutorial OptionsMenu })
            [ header model
            , HE.svg [ HA.class' "svg-feedback", HA.viewBox "0 0 752 752", HA.onClick <<< SpecialRequest $ ToggleModal ShowFeedback ]
                    [ HE.title "Send feedback, report a bug or ask anything about MeroChat"
                    , HE.path' [ HA.d "m515 232.9h-278.07c-17.84 0-32.363 14.523-32.363 32.363v246.97c0 2.5273 1.3438 4.8945 3.5508 6 1.0273 0.55078 2.1328 0.78906 3.3164 0.78906 1.3438 0 2.6055-0.31641 3.7109-1.1055l58.805-38.281h240.97c17.918 0 32.441-14.602 32.441-32.441v-181.94c0.078125-17.836-14.523-32.359-32.363-32.359zm-152.49 102.53-51.383 51.383c-1.3438 1.3438-3.0781 1.9727-4.8164 1.9727-1.7383 0-3.4727-0.71094-4.8164-1.9727l-22.891-22.891c-2.6836-2.6836-2.6836-7.0234 0-9.707s7.0234-2.6836 9.707 0l18.074 18.074 46.57-46.57c2.6836-2.6836 7.0234-2.6836 9.707 0 2.5312 2.6875 2.5312 7.0273-0.15234 9.7109zm110.9 41.754c2.6836 2.6836 2.6836 7.0234 0 9.707-1.3438 1.3438-3.0781 1.9727-4.8164 1.9727-1.7383 0-3.4727-0.71094-4.8164-1.9727l-20.836-20.836-20.836 20.836c-1.3438 1.3438-3.0781 1.9727-4.8164 1.9727s-3.4727-0.71094-4.8164-1.9727c-2.6836-2.6836-2.6836-7.0234 0-9.707l20.836-20.836-20.836-20.836c-2.6836-2.6836-2.6836-7.0234 0-9.707 2.6836-2.6836 7.0234-2.6836 9.707 0l20.836 20.836 20.836-20.836c2.6836-2.6836 7.0234-2.6836 9.707 0 2.6836 2.6836 2.6836 7.0234 0 9.707l-20.988 20.758z" ]
                    ]
            , HE.svg [ HA.class' { "svg-experiment": true }, HA.viewBox "0 0 1479 1536", HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments ]
                    [ HE.title "Chat experiments"
                    ,
                      --from https://leungwensen.github.io/svg-icon/#awesome
                      HE.path' $ HA.d "M1434.5 1320q56 89 21.5 152.5t-140.5 63.5h-1152q-106 0-140.5-63.5T44.5 1320l503-793V128h-64q-26 0-45-19t-19-45 19-45 45-19h512q26 0 45 19t19 45-19 45-45 19h-64v399zm-779-725l-272 429h712l-272-429-20-31V128h-128v436z"
                    ]
            , HE.div [ HA.class' "outer-user-menu", HA.title "Your options" ]
                    [ HE.svg [ HA.id $ show UserContextMenu, HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 16 16" ]
                            ( SIS.contextMenuElements <>
                                    [ HE.rect' [ HA.class' "strokeless", HA.x "0.03", HA.y "7", HA.width "15.93", HA.height "2" ]
                                    , HE.rect' [ HA.class' "strokeless", HA.x "0.03", HA.y "2.5", HA.width "15.93", HA.height "2" ]
                                    , HE.rect' [ HA.class' "strokeless", HA.x "0.03", HA.y "11.5", HA.width "15.93", HA.height "2" ]
                                    ]
                            )
                    , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowUserContextMenu } ]
                            [ HE.div (HA.class' "mobile-profile-header") $ header model
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowProfile
                                    , HE.span (HA.class' "duller") "Set your profile picture, name"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowSettings
                                    , HE.span (HA.class' "duller") "Change email, password, etc"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowKarmaPrivileges ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowKarmaPrivileges
                                    , HE.span (HA.class' "duller") "See your privileges, karma stats"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowExperiments
                                    , HE.span (HA.class' "duller") "Talk in novel ways"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowBacker ]
                                    [ HE.div (HA.class' "menu-item-heading") $ show ShowBacker
                                    , HE.span (HA.class' "duller") "Donate or become a patron"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp ]
                                    [ HE.div (HA.class' "menu-item-heading") "Help"
                                    , HE.span (HA.class' "duller") "Learn more about MeroChat"
                                    ]
                            , HE.div [ HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowFeedback ]
                                    [ HE.div (HA.class' "menu-item-heading") "Send feedback"
                                    , HE.span (HA.class' "duller") "Report issues, or leave a question"
                                    ]
                            , if temporary then
                                    HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmTerminationTemporaryUser ] "Delete my data"
                              else
                                    HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmLogout ] "Logout"
                            ]
                    ]
            , HE.span [ HA.class' "suggestions-button", HA.onClick $ ToggleInitialScreen false ] $
                    HE.svg [ HA.class' "svg-suggestion-button", HA.viewBox "0 0 16 16" ]
                          [ HE.path' [ HA.fill "#c3d365", HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM4.42,10l-.06.07-.49.59,0,.05-.93,1.13V9.54h0V3.41a.68.68,0,0,1,.61-.73h8.14a.67.67,0,0,1,.59.73V8.86a.72.72,0,0,1-.64.78H4.72Zm9.18,3.33-.92-1-.52-.57-.41-.45H4.9l.76-1h6.15a1,1,0,0,0,1-.9V5.11H13a.63.63,0,0,1,.61.66v5.49h0Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M12.26,8.86V3.41a.67.67,0,0,0-.59-.73H3.53a.68.68,0,0,0-.61.73V9.54h0v2.31l.93-1.13,0-.05.49-.59L4.42,10l.3-.37h6.9A.72.72,0,0,0,12.26,8.86Zm-1-4.11A3.62,3.62,0,0,1,7.68,8.23,3.66,3.66,0,0,1,5.11,7.16a3.59,3.59,0,0,1-1.05-2.4V4.1h7.26Z" ]
                          , HE.path' [ HA.fill "#c3d365", HA.class' "strokeless", HA.d "M4.06,4.76a3.59,3.59,0,0,0,1.05,2.4A3.66,3.66,0,0,0,7.68,8.23,3.62,3.62,0,0,0,11.3,4.75l0-.65H4.06Zm5.55.48a.43.43,0,1,1-.43.43A.43.43,0,0,1,9.61,5.24Zm-1.93,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Zm-1.92,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M5.76,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M7.68,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M9.61,6.1a.43.43,0,1,0-.43-.43A.43.43,0,0,0,9.61,6.1Z" ]
                          , HE.path' [ HA.fill "#262626", HA.class' "strokeless", HA.d "M13.59,5.77A.63.63,0,0,0,13,5.11h-.2V9.38a1,1,0,0,1-1,.9H5.66l-.76,1h6.85l.41.45.52.57.92,1V11.26h0Z" ]
                          ]
            ]

header ∷ ImModel → Html ImMessage
header model@{ user: { karma, karmaPosition } } = HE.fragment
      [ HE.img [ HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.title "Edit your profile", HA.class' "avatar-settings", HA.src $ SA.avatarForSender avatar ]
      , HE.div [ HA.class' "settings-name" ]
              [ HE.strong_ name
              , HE.div [ HA.class' "settings-karma", HA.onClick <<< SpecialRequest $ ToggleModal ShowKarmaPrivileges, HA.title "See your privileges and karma stats" ]
                      [ HE.span [ HA.class' "karma-number" ] $ SI.thousands karma
                      , HE.span [ HA.class' "karma-text" ] " karma "
                      , HE.span_ $ "(#" <> show karmaPosition <> ")"
                      ]
              ]
      ]
      where
      { name, avatar } = model.user