module Shared.Options.File where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))
import Environment (development)

allowedMediaTypes :: HashMap String String
allowedMediaTypes = DH.fromFoldable [Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

base :: Int
base = 500

maxImageSize :: Int
maxImageSize = base * 1024

maxImageSizeKB :: String
maxImageSizeKB = show base <> " KB"

imageBasePath :: String
imageBasePath = if development then "/client/media/" else productionBasePath

productionBasePath :: String
productionBasePath = "https://static.melan.chat/file/ourmelon/"
