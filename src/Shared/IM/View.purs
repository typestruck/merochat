module Shared.IM.View where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.IM.Contact as SIC
import Shared.IM.View.Chat as SIVC
import Shared.IM.View.Contacts as SIVCN
import Shared.IM.View.History as SIVH
import Shared.IM.View.ProfileSettings as SIVP
import Shared.IM.View.Suggestion as SIVS
import Shared.IM.View.UserMenu as SIVU
import Shared.Unsafe ((!@))

view :: IMModel -> Html IMMessage
view model@{ suggestions, suggesting, chatting, contacts, profileSettingsToggle } = HE.div (HA.class' "im") [
      HE.div (HA.class' "left-box") [
            SIVU.userMenu model,
            search model,
            SIVCN.contactList model,
            logo,

            SIVP.profileSettings profileSettingsToggle
      ],
      HE.div [HA.class' "chat-box", HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile] [
            SIVS.profile model profileUser,
            SIVH.history model historyContact,
            SIVC.chat  model
      ]
]
      where Tuple profileUser historyContact =
                  case Tuple suggesting chatting of
                        Tuple Nothing (Just index) ->
                              let contact@{ user }= contacts !@ index
                              in Tuple (Just user) $ Just contact
                        Tuple (Just index) _ ->
                              let user@{ id } = suggestions !@ index
                              in Tuple (Just user) <<< Just $ SIC.defaultContact id user
                        _ -> Tuple Nothing Nothing

search :: IMModel -> Html IMMessage
search model = HE.div' $ HA.class' "search"

logo :: Html IMMessage
logo = HE.div (HA.class' "logo-contact-list") [
      HE.img $ HA.src "/client/media/logo-small.png"
]