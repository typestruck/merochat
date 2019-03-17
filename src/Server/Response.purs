module Server.Response(html, json, serveDevelopmentFile, badRequest) where

import Prelude
import Server.Types
import Shared.Types

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
import HTTPure (Headers, ResponseM, Response)
import HTTPure as H
import HTTPure.Body (class Body)
import Node.FS.Aff as FS
import Node.Path as P
import Partial.Unsafe as U
import Run (Run, AFF)
import Run as R

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

--ok' :: forall a. Body a => Headers -> a -> ResponseEffect
ok' headers = R.liftAff <<< H.ok' headers

--html :: String -> ResponseEffect
html contents = ok' (headerContentType $ show HTML) contents

json :: forall a b c. Generic b a => EncodeRep a => b -> Run (aff :: AFF | c) Response
json value = ok' (headerContentType $ show JSON) <<< C.stringify $ EGR.genericEncodeJson value

serveDevelopmentFile :: forall c. String -> String -> Run (aff :: AFF | c) Response
serveDevelopmentFile folder fileName = do
      	contents <- R.liftAff <<< FS.readFile $ "src/Client/" <> folder <> "/" <> fileName
      	ok' (contentTypeFromExtension fileName) contents
-- serveDevelopmentFile folder fileName = catchError read (const (R.liftAff H.notFound))
--  	where read = do
--       		contents <- R.liftAff <<< FS.readFile $ "src/Client/" <> folder <> "/" <> fileName
--       		ok' (contentTypeFromExtension fileName) contents

contentTypeFromExtension :: String -> Headers
contentTypeFromExtension = headerContentType <<< show <<< read' <<< P.extname
	where   read' :: String -> ContentType
		read' = U.unsafePartial M.fromJust <<< read

headerContentType :: String -> Headers
headerContentType = H.header "Content-Type"

--badRequest :: String -> ResponseEffect
badRequest message = R.liftAff <<< H.badRequest' (headerContentType $ show JSON) <<< C.stringify <<< EGR.genericEncodeJson $ BadRequest { reason : message}