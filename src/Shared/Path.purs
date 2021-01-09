module Shared.Path (pathery, imageBasePath) where

import Prelude

import Data.Array as DA
import Data.String (Pattern(..))
import Data.String as DS
import Environment (development)
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

imageBasePath :: String
imageBasePath = if development then "/client/media/" else productionBasePath

productionBasePath :: String
productionBasePath = "https://static.melan.chat/file/staticmelan/"

fileName :: String -> String
fileName file
    | development = SU.fromJust <<< DA.head $ DS.split (Pattern ".") file
    | otherwise = file