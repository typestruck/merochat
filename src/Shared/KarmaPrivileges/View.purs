module Shared.KarmaPrivileges.View where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.KarmaPrivileges.Types

view ∷ KarmaPrivilegesModel → Html KarmaPrivilegesMessage
view { top10, inBetween10, userPosition, toggleBoard } =
      HE.div (HA.class' "karma-leaderboard") $
            HE.div (HA.class' "modal-section leaderboard ")
                  [ HE.div (HA.class' "modal-part")
                          [ HE.div (HA.class' "section-label")
                                  [ HE.div (HA.class' "bold") "Privileges"
                                  , HE.div (HA.class' "duller")
                                          [ HE.div_ "Features you have"
                                          , HE.div_ "unlocked on MeroChat"
                                          ]
                                  ]
                          , HE.div (HA.class' "margin-m")
                                  [ HE.div (HA.class' "center")
                                          [ HE.text "list"
                                          ]
                                  ]
                          ]
                  , HE.div (HA.class' "modal-part")
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
                                  , HE.div (HA.class' { "hidden": toggleBoard == InBetween10 }) $ DA.mapWithIndex createEntry top10
                                  , HE.div (HA.class' { "hidden": toggleBoard == Top10 }) $ DA.mapWithIndex createEntry inBetween10
                                  ]
                          ]
                  ]
      where
      createEntry index { position, avatar, name, karma } =
            HE.div (HA.class' $ "board-position" <> if position == userPosition then " user" else "")
                  [ HE.div (HA.class' "avatar-leaderboard-div") $ HE.img [ HA.class' $ "avatar-leaderboard" <> SA.avatarColorClass (Just index), HA.src $ SA.avatarForRecipient (Just index) avatar ]
                  , HE.div (HA.class' "name-karma")
                          [ HE.div_
                                  [ HE.div (HA.class' "name") name
                                  , HE.span (HA.class' "karma") $ show karma
                                  ]
                          , HE.div (HA.class' "position") <<< HE.span (HA.class' "position-number") $ show position
                          ]

                  ]