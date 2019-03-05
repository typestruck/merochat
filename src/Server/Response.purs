module Response(html, serveDevelopmentFile) where

import HTTPure as H
import HTTPure (ResponseM, Headers)
import Node.Encoding(Encoding(..))
import Data.String as S
import Effect.Aff (catchError)
import Node.FS.Aff as FS
import Effect.Class(liftEffect)
import Effect.Console(log)
import Prelude(bind, ($), (<>), const, discard)
import Node.Path as P
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either as E
import Shared.Common as SC
import Data.String.Read(class Read)

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
html contents = H.ok' (show HTML) contents

json :: forall a. EncodeJson a => a -> ResponseM
json value = E.either SC.parseJSONError (H.ok' (show JSON)) $ encodeJson value

serveDevelopmentFile :: String -> String -> ResponseM
serveDevelopmentFile folder fileName = catchError read (const H.notFound)
 	where read = do
      		contents <- FS.readFile $ "src/Client/" <> folder <> "/" <> fileName
      		H.ok' (contentTypeFromExtension fileName) contents

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension path = H.header "Content-Type" <<< show <<< read <<< P.extname
