module Shared.Im.View where

import Prelude
import Shared.Im.Types

import Data.Maybe as DM
import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Im.View.ChatHistory as SIVH
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.ContactList as SIVCN
import Shared.Im.View.LogoMenu as SIVL
import Shared.Im.View.Modals as SIVM
import Shared.Im.View.Notification as SIVN
import Shared.Im.View.NotificationMobile as SIVNM
import Shared.Im.View.SuggestionProfile as SIVP
import Shared.Im.View.UserMenu as SIVU

view ∷ Boolean → ImModel → Html ImMessage
view isClientRender model = HE.div "im"
      [ HE.div (HA.class' { "contact-box": true, "same-background" : DM.isJust model.chatting, "current-mobile-screen": model.initialScreen })
              [ SIVU.userMenu model
              , HE.div (HA.class' { "suggestion-box-error": true, "error-message-connection-lost": true, flexed: model.smallScreen && not (DS.null model.errorMessage) }) model.errorMessage
              , SIVN.reloadPage model.imUpdated
              , SIVN.prompt model.enableNotificationsVisible
              , SIVCN.contactList isClientRender model
              , SIVL.logoMenu model
              , SIVM.modals model
              ]
      , HE.div [ HA.class' { "suggestion-box": true, "current-mobile-screen": not model.initialScreen }, HA.onDragenter' PreventStop, HA.onDragover' PreventStop, HA.onDrop' DropFile ]
              [ HE.div [ HA.class' { "suggestion-box-error": true, "error-message-connection-lost": true, flexed: not $ DS.null model.errorMessage } ] model.errorMessage
              , SIVNM.unreadNotification model
              , SIVP.suggestionProfile model
              , SIVH.chatHistory model
              , SIVC.chat model
              ]
      ]
