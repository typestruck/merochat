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

html :: String -> ResponseM
html contents = H.ok' (contentType "_.html") contents

serveDevelopmentFile :: String -> String -> ResponseM
serveDevelopmentFile folder fileName = catchError read (const H.notFound)
 	where read = do
      		contents <- FS.readFile $ "src/client/" <> folder <> "/" <> fileName
      		H.ok' (contentType fileName) contents

contentType :: String -> Headers
contentType path = H.header "Content-Type" mediaType
  	where   mediaType =
			case S.toLower $ P.extname path of
		                    ".json" -> "application/json"
				    ".js" -> "application/javascript"
				    ".gif" -> "image/gif"
				    ".jpeg" -> "image/jpeg"
				    ".jpg" -> "image/jpeg"
				    ".png" -> "image/png"
				    ".css" -> "text/css"
				    ".html" -> "text/html"
				    _ -> "application/octet-stream"