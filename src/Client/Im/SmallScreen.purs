module Client.Im.SmallScreen where

import Prelude

import Data.Tuple.Nested ((/\))
import Client.Ui.Window as CUW
import Client.Im.Flame (NoMessages)
import Effect (Effect)
import Flame.Subscription as FS
import Shared.Breakpoint (mobileBreakpoint)
import Shared.Im.Types
import Client.AppId (imAppId)

-- | Keep track of mobile (-like) screens for things that cannot be done with media queries
checkSmallScreen ∷ Effect Boolean
checkSmallScreen = do
      width ← CUW.screenWidth
      pure $ width < mobileBreakpoint

sendSmallScreen ∷ Effect Unit
sendSmallScreen = FS.send imAppId SetSmallScreen

setSmallScreen ∷ ImModel → NoMessages
setSmallScreen model = model
      { messageEnter = false
      , smallScreen = true
      } /\ []
