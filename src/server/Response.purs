module Response(html, serveDevelopmentFile) where

import HTTPure as H
import HTTPure.Headers as HH
import HTTPure (ResponseM)

import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common as MediaType
import Data.String as String
import Effect.Aff (Aff, catchError)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path

html :: String -> ResponseM
html contents = H.ok' headers contents
	where headers = HH.header "Content-Type" "text/html"

serveDevelopmentFile :: String -> String -> ResponseM
serveDevelopmentFile = ?hole

serveFile :: FilePath -> Aff HTTPure.Response
serveFile path =
  catchError read (const HTTPure.notFound)
  where
    read = do
      contents <- FS.readFile path
      HTTPure.binaryResponse' 200 (contentType path) contents

contentType :: FilePath -> HTTPure.Headers
contentType path =
  HTTPure.header "Content-Type" mediaType
  where
    (MediaType mediaType) = extMediaType (Path.extname path)

extMediaType :: FilePath -> MediaType
extMediaType ext =
  case String.toLower ext of
    ".json" -> MediaType.applicationJSON
    ".js" -> MediaType.applicationJavascript
    ".gif" -> MediaType.imageGIF
    ".jpeg" -> MediaType.imageJPEG
    ".jpg" -> MediaType.imageJPEG
    ".png" -> MediaType.imagePNG
    ".csv" -> MediaType.textCSV
    ".html" -> MediaType.textHTML
    ".htm" -> MediaType.textHTML
    ".txt" -> MediaType.textPlain
    ".xml" -> MediaType.textXML
    _ -> MediaType.applicationOctetStream