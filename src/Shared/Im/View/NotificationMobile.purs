module Shared.Im.View.NotificationMobile where

import Prelude
import Shared.Im.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA

unreadNotification ∷ ImModel → Html ImMessage
unreadNotification model
      | model.smallScreen =
              HE.div [ HA.onClick $ ToggleInitialScreen true, HA.title "Back to contact list", HA.class' { "mobile-notification": true, hidden: DA.null avatars } ] $ HE.span [HA.class' "notification-header"] [HE.text "New messages from  "] : avatars

              where
              unreadChats = map markup $ DA.filter unread model.contacts
              unread entry = DF.any (\m → m.status < Read && m.sender /= model.user.id && Just m.sender /= model.chatting) entry.history
              markup entry = HE.img [ HA.class' $ "avatar-notification-mobile", HA.src $ SA.fromAvatar entry.user ]

              avatars
                    | DA.length unreadChats > 5 = DA.snoc (DA.take 5 unreadChats) $ HE.text "..."
                    | otherwise = unreadChats

      | otherwise = emptyDiv

emptyDiv ∷ Html ImMessage
emptyDiv = HE.div [] []