module Shared.Im.Unread where

import Prelude

import Data.Foldable as DF
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Shared.Im.Types (Contact, MessageStatus(..))
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP

title ∷ Int → String
title =
      case _ of
            0 → defaultTitle
            n → "(" <> show n <> ") " <> defaultTitle
      where
      defaultTitle = "MeroChat - Friendly Random Chat"

favicon ∷ Int → String
favicon =
      case _ of
            0 → SP.mediaPath Favicon Ico
            1 ->  SP.mediaPath Favicon1 Ico
            2 ->  SP.mediaPath Favicon2 Ico
            3 ->  SP.mediaPath Favicon3 Ico
            4 ->  SP.mediaPath Favicon4 Ico
            5 ->  SP.mediaPath Favicon5 Ico
            6 ->  SP.mediaPath Favicon6 Ico
            7 ->  SP.mediaPath Favicon7 Ico
            8 ->  SP.mediaPath Favicon8 Ico
            9 ->  SP.mediaPath Favicon9 Ico
            10 ->  SP.mediaPath Favicon10 Ico
            _ ->  SP.mediaPath Favicon10Plus Ico

countUnreadChats ∷ Int → Array Contact → Int
countUnreadChats id = DF.foldl count 0
      where
      count total { history }
            | DF.any isUnread history = total + 1
            | otherwise = total
      isUnread { sender, status } = status < Read && sender /= id
