module Shared.IM.View where

import Prelude

import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.View.Chat as SIVC
import Shared.IM.View.ContactList as SIVCN
import Shared.IM.View.ChatHistory as SIVH
import Shared.IM.View.LogoMenu as SIVL
import Shared.IM.View.Modals as SIVM
import Shared.IM.View.Notification as SIVN
import Shared.IM.View.SuggestionProfile as SIVP
import Shared.IM.View.SuggestionCall as SIVS
import Shared.IM.View.UserMenu as SIVU
import Shared.Unsafe ((!@))
import Shared.IM.Types

view ∷ Boolean → IMModel → Html IMMessage
view isClientRender model@{ enableNotificationsVisible, errorMessage, fortune, initialScreen, chatting, contacts, imUpdated, smallScreen } = HE.div [ HA.class' "im" ]
      [ HE.div (HA.class' { "contact-box": true, "current-mobile-screen": initialScreen })
              [ SIVU.userMenu model
              , SIVN.reloadPage imUpdated
              , SIVN.prompt $ not smallScreen && enableNotificationsVisible
              , SIVS.suggestionCall model
              , SIVCN.contactList isClientRender model
              , SIVL.logoMenu fortune
              , SIVM.modals model
              ]
      , HE.div [ HA.class' { "suggestion-box": true, "current-mobile-screen": not initialScreen }, HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile ]
              [ HE.div (HA.class' { "suggestion-box-error": true, flexed: not $ DS.null errorMessage }) errorMessage
              , SIVP.suggestionProfile model
              , SIVH.chatHistory model $ map (contacts !@ _) chatting
              , SIVC.chat model
              ]
      ]
