module Shared.Options.MountPoint where

import Flame (AppId(..))
import Shared.Types (IMMessage, MountPoint(..), ProfileMessage)

profileID :: AppId MountPoint ProfileMessage
profileID = AppId Profile

imID :: AppId MountPoint IMMessage
imID = AppId IM