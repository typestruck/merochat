module Shared.IM.View where

import Prelude
import Shared.Types

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.View.Chat as SIVC
import Shared.IM.View.Contacts as SIVCN
import Shared.IM.View.History as SIVH
import Shared.IM.View.Profile as SIVP
import Shared.IM.View.Retry as SIVR
import Shared.IM.View.Suggestions as SIVS
import Shared.IM.View.UserMenu as SIVU
import Shared.Unsafe ((!@))

view :: Boolean -> IMModel -> Html IMMessage
view isClientRender model@{ fortune, suggestions, suggesting, chatting, contacts, hasTriedToConnectYet, isWebSocketConnected, toggleModal } = HE.div (HA.class' "im") [
      HE.div (HA.class' "left-box") [
            SIVU.userMenu model,
            SIVS.suggestions model,
            search model,
            SIVCN.contactList isClientRender model,
            logo fortune,

            modals model
      ],
      HE.div [HA.class' "chat-box", HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile] [
            HE.div (HA.class' {"no-connection": true, flexed: hasTriedToConnectYet && not isWebSocketConnected}) "Connection to the server lost. Attempting to automaticaly reconnect...",
            SIVP.profile model,
            SIVH.history model $ map (contacts !@ _ ) chatting,
            SIVC.chat model
      ]
]

search :: IMModel -> Html IMMessage
search model = HE.div' $ HA.class' "search"

logo :: Maybe String -> Html IMMessage
logo fortune = HE.div (HA.class' "relative") [
      HE.div (HA.class' {fortune: true, hidden: DM.isNothing fortune }) [
            HE.div (HA.class' "fortune-deets") [
                  HE.text $ DM.fromMaybe "" fortune
            ],
            HE.svg [HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ] [
                  HE.title "Close",
                  HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
            ]
      ],

      HE.div [HA.class' "logo-contact-list", HA.onDblclick $ ToggleFortune true] $
            HE.img [
                  HA.createAttribute "srcset" "/client/media/logo-3-small.png 180w, /client/media/logo-small.png 210w",
                  HA.createAttribute "sizes" "(max-width: 1920px) 180px, 210px",
                  HA.src "/client/media/logo.png"
            ]
]

modals :: IMModel -> Html IMMessage
modals { toggleModal: toggle, failedRequests} =
      HE.div (HA.class' {"modal-placeholder-overlay": true, "hidden" : toggle == HideUserMenuModal}) [
            if toggle == ConfirmLogout then
                  HE.div (HA.class' "confirmation" ) [
                        HE.span (HA.class' "bold") "Do you really want to log out?",
                        HE.div (HA.class' "buttons") [
                              HE.button [HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal] "Cancel",
                              HE.button [HA.class' "green-button danger", HA.onClick Logout] "Logout"
                        ]
                  ]
            else
                  HE.div (HA.class' "modal-placeholder") [
                        HE.div (HA.class' "modal-menu") [
                              HE.div [HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal, HA.class' "back"] [
                                    HE.svg [HA.class' "svg-16", HA.viewBox "0 0 512 512"] [
                                          HE.polygon' [HA.points "496 159.961 295.983 159.961 295.976 16.024 257.698 16.024 17.364 255.706 257.313 495.941 296.001 495.976 295.993 351.961 496 351.961 496 159.961"]
                                    ],
                                    HE.text " Back to chats"
                              ],
                              HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.class' { entry: true, selected: toggle == ShowProfile }] "Your profile",
                              HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings, HA.class' { entry: true, selected: toggle == ShowSettings }] "Your settings",
                              HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard, HA.class' { entry: true, selected: toggle == ShowLeaderboard }] "Karma leaderboard",
                              HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp, HA.class' { entry: true, selected: toggle == ShowHelp }] "Help"
                        ],
                        HE.div [HA.id "profile-edition-root", HA.class' { hidden: toggle /= ShowProfile }] $ retry ShowProfile,
                        HE.div [HA.id "settings-edition-root", HA.class' { hidden: toggle /= ShowSettings }] $ retry ShowSettings,
                        HE.div [HA.id "karma-leaderboard-root", HA.class' { hidden: toggle /= ShowLeaderboard }] $ retry ShowLeaderboard,
                        HE.div [HA.id "help-root", HA.class' { hidden: toggle /= ShowHelp }] $ retry ShowHelp
                  ]
      ]
      where retry toggle = HE.div (HA.class' "retry-modal") [
            SIVR.retry "Failed to load contents" (ToggleModal toggle) failedRequests,
            HE.div' (HA.class' "loading")
      ]