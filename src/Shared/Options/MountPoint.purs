module Shared.Options.MountPoint where

import Prelude
import Shared.IM.Types (IMMessage)
import Flame (AppId(..))
import Shared.Profile.Types (ProfileMessage)

data MountPoint = IM | Profile

profileId ∷ AppId MountPoint ProfileMessage
profileId = AppId Profile

imId ∷ AppId MountPoint IMMessage
imId = AppId IM

instance Show MountPoint where
      show = case _ of
            IM → "im-mount"
            Profile → "profile-mount"
