module Shared.Options.MountPoint where

import Prelude
import Shared.Im.Types (ImMessage)
import Flame (AppId(..))
import Shared.Profile.Types (ProfileMessage)

data MountPoint = IM | Profile

profileId ∷ AppId MountPoint ProfileMessage
profileId = AppId Profile

imId ∷ AppId MountPoint ImMessage
imId = AppId IM

instance Show MountPoint where
      show = case _ of
            IM → "im-mount"
            Profile → "profile-mount"
