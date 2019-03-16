module Server.Response(html, json, serveDevelopmentFile, badRequest) where

import Prelude

import Data.Argonaut.Core as C
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.String as S
import Data.String.Read (class Read, read)
import Effect.Aff (catchError)
import HTTPure (ResponseM, Headers)
import HTTPure as H
import Node.FS.Aff as FS
import Node.Path as P
import Partial.Unsafe as U

data ContentType = JSON | JS | GIF | JPEG | PNG | CSS | HTML | OctetStream

instance contentTypeShow :: Show ContentType where
	show JSON = "application/json"
	show JS = "application/javascript"
	show GIF = "image/gif"
	show JPEG = "image/jpeg"
	show PNG = "image/png"
	show CSS = "text/css"
	show HTML = "text/html"
	show _ = "application/octet-stream"

-- match both on file extension or content type
instance contentTypeRead :: Read ContentType where
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
		where value = S.trim $ S.toLower v

html :: String -> ResponseM
html contents = H.ok' (headerContentType $ show HTML) contents

--json :: forall a. EncodeJson a => a -> ResponseM
json value = H.ok' (headerContentType $ show JSON) <<< C.stringify $ EGR.genericEncodeJson value

serveDevelopmentFile :: String -> String -> ResponseM
serveDevelopmentFile folder fileName = catchError read (const H.notFound)
 	where read = do
      		contents <- FS.readFile $ "src/Client/" <> folder <> "/" <> fileName
      		H.ok' (contentTypeFromExtension fileName) contents

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read' <<< P.extname
	where   read' :: String -> ContentType
		read' = U.unsafePartial M.fromJust <<< read

headerContentType :: String -> Headers
headerContentType = H.header "Content-Type"

badRequest :: String -> ResponseM
badRequest message = H.badRequest' (headerContentType $ show JSON) <<< hole? $ BadRequest {reason :: message}