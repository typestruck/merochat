module Shared.ContentType where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as DS
import Data.String.Read (class Read)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)


newtype Html = Html String

-- data ContentType = JSON
--       | Js | GIF | JPEG | Png | Css | HTML | OctetStream

-- instance Show ContentType where
--       show JSON = "application/json"
--       show Js = "application/javascript"
--       show GIF = "image/gif"
--       show JPEG = "image/jpeg"
--       show Png = "image/png"
--       show Css = "text/css"
--       show HTML = "text/html"
--       show _ = "application/octet-stream"

-- -- match both on file extension or content type
-- instance contentTypeRead ∷ Read ContentType where
--       read v =
--             Just $
--                   if value == ".json" || value == show JSON then JSON
--                   else if value == ".js" || value == show Js then Js
--                   else if value == ".gif" || value == show GIF then GIF
--                   else if value == ".jpeg" || value == ".jpg" || value == show JPEG then JPEG
--                   else if value == ".png" || value == show Png then Png
--                   else if value == ".css" || value == show Css then Css
--                   else if value == ".html" || value == show HTML then HTML
--                   else OctetStream
--             where
--             value = DS.trim $ DS.toLower v

derive instance Newtype Html _

instance EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response
            { headers: PH.setIfNotDefined "content-type" html headers
            , body: PR.StringBody contents
            , status
            }