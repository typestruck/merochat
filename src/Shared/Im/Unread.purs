module Shared.Im.Unread where

import Prelude

import Data.Either (Either(..))
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
            0 → SP.resourcePath (Left Favicon) Ico
            1 → SP.resourcePath (Left Favicon1) Ico
            2 → SP.resourcePath (Left Favicon2) Ico
            3 → SP.resourcePath (Left Favicon3) Ico
            4 → SP.resourcePath (Left Favicon4) Ico
            5 → SP.resourcePath (Left Favicon5) Ico
            6 → SP.resourcePath (Left Favicon6) Ico
            7 → SP.resourcePath (Left Favicon7) Ico
            8 → SP.resourcePath (Left Favicon8) Ico
            9 → SP.resourcePath (Left Favicon9) Ico
            10 → SP.resourcePath (Left Favicon10) Ico
            _ → SP.resourcePath (Left Favicon10Plus) Ico

countUnreadChats ∷ Int → Array Contact → Int
countUnreadChats id = DF.foldl count 0
      where
      count total { history }
            | DF.any isUnread history = total + 1
            | otherwise = total
      isUnread { sender, status } = status < Read && sender /= id
