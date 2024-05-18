module Shared.Avatar where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU

defaultAvatar ∷ String
defaultAvatar = avatarPath 1

differentAvatarImages ∷ Int
differentAvatarImages = 8

avatarPath ∷ Int → String
avatarPath index = SP.mediaPath name Png
      where
      name = case index of
            1 → Avatar1
            2 → Avatar2
            3 → Avatar3
            4 → Avatar4
            5 → Avatar5
            6 → Avatar6
            7 → Avatar7
            _ → Avatar8

avatarForSender ∷ Maybe String → String
avatarForSender = DM.fromMaybe defaultAvatar

avatarForRecipient ∷ Maybe Int → Maybe String → String
avatarForRecipient index = DM.fromMaybe (avatarPath <<< avatarIndex $ SU.fromJust index)

avatarIndex ∷ Int → Int
avatarIndex index = mod index differentAvatarImages + 1

avatarColorClass ∷ Maybe Int → String
avatarColorClass index = className <> show (mod (SU.fromJust index) totalColorClasses + 1)
      where
      className = " avatar-color-"
      totalColorClasses = 4

parseAvatar ∷ Maybe String → Maybe String
parseAvatar avatar = (\a → SP.mediaPath (Upload a) Included) <$> avatar