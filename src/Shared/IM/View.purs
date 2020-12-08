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
import Shared.IM.View.LogoMenu as SIVL
import Shared.IM.View.Modals as SIVM
import Shared.IM.View.Notification as SIVN

view :: Boolean -> IMModel -> Html IMMessage
view isClientRender model@{ enableNotificationsVisible, fortune, initialScreen, suggestions, suggesting, chatting, contacts, hasTriedToConnectYet, isWebSocketConnected, toggleModal } = HE.div (HA.class' "im") [
      HE.div (HA.class' {"contact-box": true, "current-mobile-screen": initialScreen} ) [
            SIVU.userMenu model,
            SIVN.prompt enableNotificationsVisible,
            SIVS.suggestions model,
            SIVCN.contactList isClientRender model,
            SIVL.logoMenu fortune,
            SIVM.modals model
      ],
      HE.div [HA.class' {"suggestion-box" : true, "current-mobile-screen": not initialScreen } , HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile] [
            HE.div (HA.class' {"no-connection": true, flexed: hasTriedToConnectYet && not isWebSocketConnected}) "Connection to the server lost. Attempting to automaticaly reconnect...",
            SIVP.profile model,
            SIVH.history model $ map (contacts !@ _ ) chatting,
            SIVC.chat model
      ]
]

