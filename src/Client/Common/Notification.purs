module Client.Common.Notification where

import Prelude

import Effect (Effect)
import Web.HTML as WH
import Web.HTML.Window as WHW

alert :: String -> Effect Unit
alert message = do
        window <- WH.window
        WHW.alert message window