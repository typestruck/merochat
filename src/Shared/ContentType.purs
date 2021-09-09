module Shared.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.Read (class Read)

data ContentType = JSON | JS | GIF | JPEG | PNG | CSS | HTML | OctetStream

instance contentTypeShow ∷ Show ContentType where
      show JSON = "application/json"
      show JS = "application/javascript"
      show GIF = "image/gif"
      show JPEG = "image/jpeg"
      show PNG = "image/png"
      show CSS = "text/css"
      show HTML = "text/html"
      show _ = "application/octet-stream"

-- match both on file extension or content type
instance contentTypeRead ∷ Read ContentType where
      read v =
            Just $
                  if value == ".json" || value == show JSON then JSON
                  else if value == ".js" || value == show JS then JS
                  else if value == ".gif" || value == show GIF then GIF
                  else if value == ".jpeg" || value == ".jpg" || value == show JPEG then JPEG
                  else if value == ".png" || value == show PNG then PNG
                  else if value == ".css" || value == show CSS then CSS
                  else if value == ".html" || value == show HTML then HTML
                  else OctetStream
            where
            value = DS.trim $ DS.toLower v
