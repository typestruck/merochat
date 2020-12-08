module Shared.IM.View.Notification where

import Prelude
import Shared.Types

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

prompt :: Boolean -> Html IMMessage
prompt enableNotificationsVisible = HE.div [HA.class' { notification: true, hidden: not enableNotificationsVisible }, HA.onClick AskNotification] "Enable notifications"
