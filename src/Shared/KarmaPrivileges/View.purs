module Shared.KarmaPrivileges.View where

import Prelude
import Shared.KarmaPrivileges.Types

import Data.Array as DA
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Intl as SI
import Shared.Svg as SS

view ∷ KarmaPrivilegesModel → Html KarmaPrivilegesMessage
view model@{ top10, inBetween10, userPosition, toggleBoard, privileges, stats: { sent, started, karma, total } } =
      HE.div [ HA.id "karma-leaderboard", HA.class' { hidden: not model.visible } ]
            [ HE.div [ HA.class' "modal-section leaderboard " ]
                    [ HE.div [ HA.class' "modal-part" ]
                            [ HE.div [ HA.class' "section-label" ]
                                    [ HE.div [ HA.class' "bold" ] [ HE.text "Karma leaderboard" ]
                                    , HE.div [ HA.class' "duller" ] [ HE.text "Your karma ranking" ]
                                    ]
                            , HE.div [ HA.class' "margin-m" ]
                                    [ HE.div [ HA.class' "green-tab" ]
                                            [ HE.span [ HA.class' { "regular-green-tab": true, "selected-green-tab": toggleBoard == InBetween10 }, HA.onClick $ ToggleBoardDisplay InBetween10 ] [ HE.text "Your position" ]
                                            , HE.span [ HA.class' { "regular-green-tab": true, "selected-green-tab": toggleBoard == Top10 }, HA.onClick $ ToggleBoardDisplay Top10 ] [ HE.text "Top 10" ]
                                            ]
                                    , HE.div [ HA.class' { "hidden": toggleBoard == InBetween10 } ] $ DA.mapWithIndex leaderboardEntry top10
                                    , HE.div [ HA.class' { "hidden": toggleBoard == Top10 } ] $ DA.mapWithIndex leaderboardEntry inBetween10
                                    ]
                            ]
                    , HE.div [ HA.class' "modal-part" ]
                            [ HE.div [ HA.class' "section-label" ]
                                    [ HE.div [ HA.class' "bold" ] [ HE.text "Stats" ]
                                    , HE.div [ HA.class' "duller" ]
                                            [ HE.div_ [ HE.text "How you have " ]
                                            , HE.div_ [ HE.text "used MeroChat so far" ]
                                            ]
                                    ]
                            , HE.div [ HA.class' "margin-m" ]
                                    [ HE.div [ HA.class' "privilege-list" ] $ map statEntry
                                            [ Tuple karma "Total karma"
                                            , Tuple total "Total chats"
                                            , Tuple started "Chats started"
                                            , Tuple sent "Messages sent"
                                            ]
                                    ]
                            ]
                    , HE.div [ HA.class' "modal-part" ]
                            [ HE.div [ HA.class' "section-label" ]
                                    [ HE.div [ HA.class' "bold" ] [ HE.text "Privileges" ]
                                    , HE.div [ HA.class' "duller" ]
                                            [ HE.div_ [ HE.text "Features that are" ]
                                            , HE.div_ [ HE.text "unlocked with karma" ]
                                            ]
                                    ]
                            , HE.div [ HA.class' "margin-m" ]
                                    [ HE.div [ HA.class' "privilege-list" ] $ map privilegeEntry privileges
                                    ]
                            ]
                    ]
            ]
      where
      statEntry (Tuple n lbl) = HE.div_
            [ HE.div [ HA.class' "privilege-body" ]
                    [ HE.div [ HA.class' "privilege-quantity" ] [ HE.text $ SI.thousands n ]
                    , HE.div [ HA.class' "privilege-name-description" ] [ HE.text lbl ]
                    ]
            ]
      privilegeEntry { name, description, quantity, got } = HE.div_
            [ HE.div [ HA.class' "privilege-body" ]
                    [ HE.div [ HA.class' "privilege-quantity" ]
                            [ if got then
                                SS.checked "You have unlocked this feature"
                              else
                                    HE.div [ HA.title $ "You need " <> show quantity <> " karma to unlock this feature" ] [ HE.text $ show quantity ]
                            ]
                    , HE.div [ HA.class' "privilege-name-description" ]
                            [ HE.text name
                            , HE.div [ HA.class' "duller privilege-description" ] [ HE.text description ]
                            ]
                    ]
            ]

      leaderboardEntry index user =
            HE.div [ HA.class' $ "board-position" <> if user.position == userPosition then " user" else "" ]
                  [ HE.div [ HA.class' "avatar-leaderboard-div" ] [ HE.img [ HA.class' "avatar-leaderboard", HA.src $ SA.fromAvatar user ] ]
                  , HE.div [ HA.class' "name-karma" ]
                          [ HE.div_
                                  [ HE.div [ HA.class' "name" ] [ HE.text user.name ]
                                  , HE.span [ HA.class' "duller" ] [ HE.text $ SI.thousands user.karma ]
                                  ]
                          , HE.div [ HA.class' "position" ]
                                  [ HE.span [ HA.class' "position-number" ] [ HE.text $ show user.position ]
                                  ]
                          ]
                  ]