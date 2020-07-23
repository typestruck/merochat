module Shared.IM.View.Chat where

import Prelude
import Shared.IM.Types

import Data.Maybe as DM
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

chat :: IMModel -> Html IMMessage
chat (IMModel {chatting, suggesting}) =
      HE.div (HA.class' "send-box") $
            HE.div (HA.class' classes) $
                  HE.textarea' [HA.class' "chat-input-textarea", HA.placeholder "Type a message or drag files here"]
      where classes = "chat-input-textarea-options" <> if DM.isNothing chatting && DM.isNothing suggesting then " hidden" else ""
