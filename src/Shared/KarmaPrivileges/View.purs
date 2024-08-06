module Shared.KarmaPrivileges.View where

import Prelude
import Shared.KarmaPrivileges.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Shared.Intl as SI
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA

view ∷ KarmaPrivilegesModel → Html KarmaPrivilegesMessage
view { top10, inBetween10, userPosition, toggleBoard, privileges, stats: { sent, started, karma, total } } =
      HE.div (HA.class' "karma-leaderboard") $
            HE.div (HA.class' "modal-section leaderboard ")
                  [ HE.div (HA.class' "modal-part")
                          [ HE.div (HA.class' "section-label")
                                  [ HE.div (HA.class' "bold") "Karma leaderboard"
                                  , HE.div (HA.class' "duller") $ HE.text "Your karma ranking"
                                  ]
                          , HE.div (HA.class' "margin-m")
                                  [ HE.div (HA.class' "center")
                                          [ HE.span [ HA.class' { "place-link": true, "selected": toggleBoard == InBetween10 }, HA.onClick $ ToggleBoardDisplay InBetween10 ] "Your position"
                                          , HE.span (HA.class' "separator duller") "•"
                                          , HE.span [ HA.class' { "place-link": true, "selected": toggleBoard == Top10 }, HA.onClick $ ToggleBoardDisplay Top10 ] "Top 10"
                                          ]
                                  , HE.div (HA.class' { "hidden": toggleBoard == InBetween10 }) $ DA.mapWithIndex leaderboardEntry top10
                                  , HE.div (HA.class' { "hidden": toggleBoard == Top10 }) $ DA.mapWithIndex leaderboardEntry inBetween10
                                  ]
                          ]
                  , HE.div (HA.class' "modal-part")
                          [ HE.div (HA.class' "section-label")
                                  [ HE.div (HA.class' "bold") "Stats"
                                  , HE.div (HA.class' "duller")
                                          [ HE.div_ "How you have "
                                          , HE.div_ "used MeroChat so far"
                                          ]
                                  ]
                          , HE.div (HA.class' "margin-m")
                                  <<< HE.div (HA.class' "privilege-list") $ map statEntry
                                  [ Tuple karma "Total karma"
                                  , Tuple total "Total chats"
                                  , Tuple started "Chats started"
                                  , Tuple sent "Messages sent"
                                  ]
                          ]
                  , HE.div (HA.class' "modal-part")
                          [ HE.div (HA.class' "section-label")
                                  [ HE.div (HA.class' "bold") "Privileges"
                                  , HE.div (HA.class' "duller")
                                          [ HE.div_ "Features that are"
                                          , HE.div_ "unlocked with karma"
                                          ]
                                  ]
                          , HE.div (HA.class' "margin-m")
                                  [ HE.div (HA.class' "privilege-list") $ map privilegeEntry privileges
                                  ]
                          ]

                  ]
      where
      statEntry (Tuple n lbl) = HE.div_
            [ HE.div (HA.class' "privilege-body")
                    [ HE.div (HA.class' "privilege-quantity") $ SI.thousands n
                    , HE.div (HA.class' "privilege-name-description") lbl
                    ]
            ]
      privilegeEntry { name, description, quantity, got } = HE.div_
            [ HE.div (HA.class' "privilege-body")
                    [ HE.div (HA.class' "privilege-quantity")
                            [ if got then
                                    HE.svg [ HA.viewBox "0 0 16 16" ]
                                          [ HE.title "You have unlocked this feature"
                                          , HE.path' [ HA.d "M10.67,5.11l-4.3,4.3L4.73,7.77a.62.62,0,0,0-.88.88l2.52,2.52L11.55,6a.62.62,0,0,0-.88-.88Z" ]
                                          ]
                              else
                                    HE.div [ HA.title $ "You need " <> show quantity <> " karma to unlock this feature" ] $ show quantity
                            ]
                    , HE.div (HA.class' "privilege-name-description")
                            [ HE.text name
                            , HE.div (HA.class' "duller privilege-description") description
                            ]
                    ]
            ]

      leaderboardEntry index { position, avatar, name, karma } =
            let
                  avatarClasses
                        | DM.isNothing avatar = "avatar-leaderboard" <> SA.avatarColorClass (Just index)
                        | otherwise = "avatar-leaderboard"
            in
                  HE.div (HA.class' $ "board-position" <> if position == userPosition then " user" else "")
                        [ HE.div (HA.class' "avatar-leaderboard-div") $ HE.img [ HA.class' avatarClasses, HA.src $ SA.avatarForRecipient (Just index) avatar ]
                        , HE.div (HA.class' "name-karma")
                                [ HE.div_
                                        [ HE.div (HA.class' "name") name
                                        , HE.span (HA.class' "duller") $ SI.thousands karma
                                        ]
                                , HE.div (HA.class' "position") <<< HE.span (HA.class' "position-number") $ show position
                                ]

                        ]