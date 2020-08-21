module Server.File where

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))

allowedMediaTypes :: HashMap String String
allowedMediaTypes = DH.fromFoldable [Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]