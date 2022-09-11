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
      defaultTitle = "MelanChat - Friendly Random Chat"

favicon ∷ Int → String
favicon =
      case _ of
            0 → file
            n | n <= 10 → DS.replace (Pattern ".") (Replacement ("-" <> show n <> ".")) file
            _ → DS.replace (Pattern ".") (Replacement ("-10-plus.")) file
      where
      file = SP.mediaPath Favicon Ico

countUnreadChats ∷ Int → Array Contact → Int
countUnreadChats id = DF.foldl count 0
      where
      count total { history }
            | DF.any isUnread history = total + 1
            | otherwise = total
      isUnread { sender, status } = status < Read && sender /= id
