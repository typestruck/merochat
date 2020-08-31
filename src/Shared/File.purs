module Shared.File where

import Prelude

base :: Int
base = 500

maxImageSize :: Int
maxImageSize = base * 1024

maxImageSizeKB :: String
maxImageSizeKB = show base <> "KB"