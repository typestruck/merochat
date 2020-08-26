module Shared.IM.View.History where

import Prelude
import Shared.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.Markdown as SM

history :: IMModel -> Maybe Contact -> Html IMMessage
history { user: { id: senderID, avatar: senderAvatar }, chatting } chattingSuggestion = HE.div [HA.class' "message-history" ] <<< HE.div [HA.class' "message-history-wrapper", HA.id "message-history-wrapper", HA.onScroll CheckFetchHistory] $
      case chattingSuggestion of
            Nothing -> [HE.createEmptyElement "div"]
            Just recipient -> display recipient

      where display recipient@{history, user: { description, avatar }} =
                  --having the description node always present avoids snabbdom choking on the use of innerHTML
                  HE.div' [HA.class' {"message": true, "description-message" : true, "hidden": not $ DA.null history }, HA.innerHTML $ SM.toHTML description] : map (entry avatar) history

            entry recipientAvatar { status, sender, content } =
                  let Tuple class' avatar =
                        if senderID == sender then Tuple "sender-message" $ SA.avatarForSender senderAvatar
                         else Tuple "recipient-message" $ SA.avatarForRecipient chatting recipientAvatar
                  in HE.div (HA.class' $ "message " <> class') [
                        HE.img [HA.src avatar, HA.class' "avatar-message"],
                        HE.div' [HA.class' $ statusClasses status, HA.innerHTML $ SM.toHTML content]
                  ]

            statusClasses = case _ of
                  Errored -> "message-failed"
                  _ -> ""


