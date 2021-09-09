module Shared.Options.MountPoint where

import Prelude
import Shared.IM.Types (IMMessage)
import Flame (AppId(..))
import Shared.Profile.Types (ProfileMessage)

data MountPoint = IM | Profile

profileID ∷ AppId MountPoint ProfileMessage
profileID = AppId Profile

imID ∷ AppId MountPoint IMMessage
imID = AppId IM

instance Show MountPoint where
      show = case _ of
            IM → "im-mount"
            Profile → "profile-mount"
