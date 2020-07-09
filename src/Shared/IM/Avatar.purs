module Client.IM.Avatar where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Shared.Unsafe as SU

defaultAvatar :: String
defaultAvatar = fileName 1

differentAvatarImages :: Int
differentAvatarImages = 8

fileName :: Int -> String
fileName index = "/client/media/avatar-" <> show index <> ".png"

avatarForSender :: Maybe String -> String
avatarForSender = DM.fromMaybe defaultAvatar


avatarForRecipient :: Maybe Int -> Maybe String -> String
avatarForRecipient index = DM.fromMaybe (fileName $ mod (SU.unsafeFromJust "view.profile" index) differentAvatarImages + 1)