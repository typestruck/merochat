module Client.Ui.Notification where

import Prelude

import Effect (Effect)

foreign import requestNotificationPermission ∷ Effect Unit → Effect Unit

foreign import notificationPermission ∷ Effect String
