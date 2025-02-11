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
unreadNotification { smallScreen, contacts, user: { id } } = HE.div [ HA.onClick $ ToggleInitialScreen true, HA.title "Back to contact list", HA.class' { "mobile-notification": true, hidden: not smallScreen || DA.null avatars } ] $ HE.span (HA.class' "notification-header") "New messages from  " : avatars
      where
      avatars =
            let
                  all = DA.catMaybes $ DA.mapWithIndex unread contacts
            in
                  if DA.length all > 5 then DA.snoc (DA.take 5 all) $ HE.text "..." else all
      unread index { history, user: { avatar } }
            | DF.any (\{ status, sender } → status < Read && sender /= id) history = Just $ HE.img [ HA.class' $ "avatar-notification-mobile" <> SA.avatarColorClass index, HA.src $ SA.avatarForRecipient index avatar ]
            | otherwise = Nothing