module Shared.IM.View.History where

import Prelude
import Shared.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.Markdown as SM

history :: IMModel -> Maybe Contact -> Html IMMessage
history { user: { id: senderID, avatar: senderAvatar }, chatting, freeToFetchChatHistory } chattingSuggestion = HE.div [HA.class' "message-history" ] <<< HE.div [HA.class' "message-history-wrapper", HA.id "message-history-wrapper", HA.onScroll CheckFetchHistory] $
      case chattingSuggestion of
            Nothing -> [HE.createEmptyElement "div"]
            Just recipient@{ shouldFetchChatHistory } ->
                  let entries = display recipient
                  in if shouldFetchChatHistory || not freeToFetchChatHistory then HE.div' (HA.class' "loading") : entries else entries

      where display recipient@{ history, user: { avatar } } = DA.mapWithIndex (\i -> entry avatar (map _.sender (history !! (i - 1)))) history

            entry recipientAvatar previousSender { status, sender, content } =
                  let   sameSenderClass = if previousSender == Just sender then " no-avatar-message" else ""
                        Tuple messageClass avatar =
                              if senderID == sender then
                                    Tuple "sender-message" $ SA.avatarForSender senderAvatar
                              else
                                    Tuple "recipient-message" $ SA.avatarForRecipient chatting recipientAvatar
                  in HE.div (HA.class' $ "message " <> messageClass <> sameSenderClass) [
                        HE.img [HA.src avatar, HA.class' $ "avatar-message" <> SA.avatarColorClass chatting],
                        HE.div (HA.class' {"exclamation": true, "hidden": status /= Errored}) "!",
                        HE.div_ [
                              HE.div' [HA.class' $ "message-content", HA.innerHTML (SM.toHTML content)],
                              HE.span (HA.class' {"error-message": true, "hidden": status /= Errored})  "Failed to send"
                        ]

                  ]

