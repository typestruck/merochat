module Shared.Path (pathery, updateHash) where

import Prelude

import Data.Array as DA
import Data.String (Pattern(..))
import Data.String as DS
import Environment (commonJSHash, development, emojiJSHash, imCSSHash, imJSHash, otherJSHash)
import Shared.Options.File (imageBasePath, productionBasePath)
import Shared.Types (ContentType(..))
import Shared.Unsafe as SU

pathery :: ContentType -> String -> String
pathery contentType file = case contentType of
    JS -> jsBasePath <> fileName file <> ".bundle.js"
    CSS -> cssBasePath <> fileName file <> ".css"
    PNG -> imageBasePath <> file <> ".png"
    _ -> file

jsBasePath :: String
jsBasePath = if development then "/client/javascript/" else productionBasePath

cssBasePath :: String
cssBasePath = if development then "/client/css/" else productionBasePath

fileName :: String -> String
fileName file
    | development = SU.fromJust <<< DA.head $ DS.split (Pattern ".") file
    | otherwise = file

updateHash :: String
updateHash = commonJSHash <> otherJSHash <> imJSHash <> emojiJSHash <> imCSSHash