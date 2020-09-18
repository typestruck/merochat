module Shared.IM.View where

import Prelude
import Shared.Types

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.IM.Contact as SIC
import Shared.IM.View.Chat as SIVC
import Shared.IM.View.Contacts as SIVCN
import Shared.IM.View.History as SIVH
import Shared.IM.View.Profile as SIVS
import Shared.IM.View.UserMenu as SIVU
import Shared.Unsafe ((!@))

view :: Boolean -> IMModel -> Html IMMessage
view displayLastMessageDates model@{ suggestions, suggesting, chatting, contacts, toggleModal } = HE.div (HA.class' "im") [
      HE.div (HA.class' "left-box") [
            SIVU.userMenu model,
            search model,
            SIVCN.contactList displayLastMessageDates model ,
            logo,

            modals toggleModal
      ],
      HE.div [HA.class' "chat-box", HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile] [
            SIVS.profile model,
            SIVH.history model $ map (contacts !@ _ ) chatting,
            SIVC.chat  model
      ]
]

search :: IMModel -> Html IMMessage
search model = HE.div' $ HA.class' "search"

logo :: Html IMMessage
logo = HE.div (HA.class' "logo-contact-list") [
      HE.img $ HA.src "/client/media/logo-small.png"
]

modals toggle = HE.div (HA.class' $ "modal-placeholder" <> if toggle /= Hidden then "" else " hidden") [
      HE.div (HA.class' "modal-menu") [
            HE.div [HA.onClick (ToggleModal Hidden), HA.class' "back"] [
                  HE.svg [HA.class' "svg-16", HA.viewBox "0 0 512 512"] [
                        HE.polygon' [HA.points "496 159.961 295.983 159.961 295.976 16.024 257.698 16.024 17.364 255.706 257.313 495.941 296.001 495.976 295.993 351.961 496 351.961 496 159.961"]
                  ],
                  HE.text " Back to chats"
            ],
            HE.div [HA.onClick (ToggleModal ShowProfile), HA.class' { entry: true, selected: toggle == ShowProfile }] "Your profile",
            HE.div [HA.onClick (ToggleModal ShowSettings), HA.class' {  entry: true, selected: toggle == ShowSettings }] "Your settings",
            HE.div [HA.onClick (ToggleModal ShowLeaderboard), HA.class' {  entry: true, selected: toggle == ShowLeaderboard }] "Karma leaderboard",
            HE.div [HA.onClick (ToggleModal ShowHelp), HA.class' {  entry: true, selected: toggle == ShowHelp }] "Help"
      ],
      HE.div [HA.id "profile-edition-root", HA.class' { hidden: toggle /= ShowProfile }] $ "Loading...",
      HE.div [HA.id "settings-edition-root", HA.class' { hidden: toggle /= ShowSettings }] $ "Loading...",
      HE.div [HA.id "karma-leaderboard-root", HA.class' { hidden: toggle /= ShowLeaderboard }] $ "Loading...",
      HE.div [HA.id "help-root", HA.class' { hidden: toggle /= ShowHelp }] $ "Loading..."
]