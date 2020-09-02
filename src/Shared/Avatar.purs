module Shared.Avatar where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Shared.Unsafe as SU

defaultAvatarName :: String
defaultAvatarName = baseFileName <> show "1" <> fileExtension

defaultAvatar :: String
defaultAvatar = fileName 1

differentAvatarImages :: Int
differentAvatarImages = 8

baseFileName :: String
baseFileName = "avatar-"

fileExtension :: String
fileExtension = ".png"

fileName :: Int -> String
fileName index = "/client/media/" <> baseFileName <> show index <> fileExtension

avatarForSender :: Maybe String -> String
avatarForSender = DM.fromMaybe defaultAvatar

avatarForRecipient :: Maybe Int -> Maybe String -> String
avatarForRecipient index = DM.fromMaybe (fileName <<< avatarIndex $ SU.fromJust index)

avatarIndex :: Int -> Int
avatarIndex index = mod index differentAvatarImages + 1

avatarColorClass :: Maybe Int -> String
avatarColorClass index = " avatar-color-" <> show (mod (SU.fromJust index) colorClasses + 1)
      where colorClasses = 4