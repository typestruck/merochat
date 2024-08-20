module Client.Im.Pwa where

import Prelude

import Client.Common.Dom (Registration)
import Client.Common.Dom as CCD
import Data.Maybe (Maybe(..))
import Data.Traversable as DT
import Effect (Effect)
import Web.HTML as WH
import Web.HTML.Window as WHW

-- | Check if merochat is running as a progressive web application
checkPwa ∷ Effect Boolean
checkPwa = do
      matches ← DT.traverse CCD.mediaMatches [ "fullscreen", "standalone", "minimal-ui" ]
      pure $ DT.or matches

registerServiceWorker ∷ Effect Registration
registerServiceWorker = do
      window ← WH.window
      navigator ← WHW.navigator window
      CCD.register navigator "/file/default/sw.js"
      CCD.ready navigator

-- | Subscribe to push notifications
subscribePush ∷ Registration → Effect Unit
subscribePush registration = do
      existing ← CCD.getSubscription registration
      case existing of
            Nothing → CCD.subscribe registration
            Just s → pure unit