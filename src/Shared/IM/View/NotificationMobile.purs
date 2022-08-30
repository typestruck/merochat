module Shared.IM.View.NotificationMobile where

import Prelude
import Shared.IM.Types (ImMessage(..), ImModel)

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA

unreadNotification ∷ ImModel → Html ImMessage
unreadNotification { smallScreen, contacts } = HE.div [ HA.onClick $ ToggleInitialScreen true, HA.title "Go back to contact list", HA.class' { "mobile-notification": true, hidden: not smallScreen } ]
      [ HE.span_ "New messages from  ",
        HE.fragment avatars
      ]
      where avatars = map (\contact ->HE.img [ HA.class' $ "avatar-notification-mobile" <> SA.avatarColorClass (Just 0) , HA.src $ SA.avatarForRecipient (Just 1) contact.user.avatar]) contacts
