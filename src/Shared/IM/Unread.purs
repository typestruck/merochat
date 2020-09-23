module Shared.IM.Unread where

import Prelude

import Data.Foldable as DF
import Shared.Types

title :: Int -> String
title =
      case _ of
            0 -> defaultTitle
            n -> "(" <> show n <> ") " <> defaultTitle
      where defaultTitle = "Melanchat (Friendly) Random Chat"

favicon :: Int -> String
favicon =
      case _ of
            0 -> name <> extension
            n -> name <> show (min 10 n) <> extension
      where name = "/client/media/favicon"
            extension = ".ico"

countUnreadChats :: PrimaryKey -> Array Contact -> Int
countUnreadChats id = DF.foldl count 0
      where count total { history }
                  | DF.any isUnread history = total + 1
                  | otherwise = total
            isUnread { sender, status } = status == Received && sender /= id
