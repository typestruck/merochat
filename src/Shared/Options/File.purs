module Shared.Options.File where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))

allowedMediaTypes ∷ HashMap String String
allowedMediaTypes = DH.fromFoldable [ Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

base ∷ Int
base = 500

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
