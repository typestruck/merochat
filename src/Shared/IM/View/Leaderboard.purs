module Shared.IM.View.Leaderboard where

import Shared.Types
import Prelude

import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

leaderboard :: Boolean -> Html IMMessage
leaderboard toggle = HE.div (HA.class' $ "modal-placeholder" <> if toggle then "" else " hidden") [
      HE.div (HA.class' "modal-menu") [
            HE.div [HA.onClick ToggleKarmaLeaderBoard] [
                  HE.svg [HA.class' "svg-32 back-arrow", HA.id "cil-arrow-thick-to-left", HA.viewBox "0 0 24 24"] [
                        HE.path' $ HA.d "M15.75 8.25v-5.625h-1.81l-9.375 9.366 9.375 9.384h1.811v-5.625h7.5v-7.5zM21.75 14.25h-7.5v5.314l-7.564-7.572 7.564-7.557v5.315h7.5z",
                        HE.path' $ HA.d "M0.75 2.625h1.5v18.75h-1.5v-18.75z"
                  ],
                  HE.text "Back to chats"
            ]
      ],
      HE.div [HA.id "karma-leaderboard-root"] $ "Loading..."
]
