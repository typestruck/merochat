module Client.Im.SmallScreen where

import Prelude

import Client.Common.Dom as CCD
import Client.Im.Flame (NoMessages)
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Breakpoint (mobileBreakpoint)
import Shared.Im.Types
import Shared.Options.MountPoint (imId)

-- | Keep track of mobile (-like) screens for things that cannot be done with media queries
checkSmallScreen ∷ Effect Boolean
checkSmallScreen = do
      width ← CCD.screenWidth
      pure $ width < mobileBreakpoint

sendSmallScreen ∷ Effect Unit
sendSmallScreen = FS.send imId SetSmallScreen

setSmallScreen ∷ ImModel → NoMessages
setSmallScreen model = F.noMessages model
      { messageEnter = false
      , smallScreen = true
      }