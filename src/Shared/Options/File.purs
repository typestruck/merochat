module Shared.Options.File where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))
import Data.Set as DS
import Data.Array as DA
import Data.Set (Set)

allowedMediaTypes ∷ HashMap String String
allowedMediaTypes = DH.fromFoldable <<< DA.zip [ "data:image/png;base64", "data:image/jpeg;base64", "data:image/tiff;base64", "data:image/bmp;base64" , "data:image/gif;base64" ] $ DS.toUnfoldable allowedExtensions

allowedExtensions ∷ Set String
allowedExtensions = DS.fromFoldable [".png", ".jpg", ".tiff", ".bmp",  ".gif" ]

base ∷ Int
base = 1000

maxImageSize ∷ Int
maxImageSize = base * 1024

maxImageSizeKB ∷ String
maxImageSizeKB = show base <> " KB"

productionBasePath ∷ String
productionBasePath = "https://static.mero.chat/file/ourmelon/"

developmentImageBasePath ∷ String
developmentImageBasePath = "/client/media/"

developmentJsBasePath ∷ String
developmentJsBasePath ="/client/javascript/"

developmentCssBasePath ∷ String
developmentCssBasePath = "/client/css/"
