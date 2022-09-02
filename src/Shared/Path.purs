module Shared.Path (pathery, updateHash) where

import Prelude

import Data.Array as DA
import Data.String (Pattern(..))
import Data.String as DS
import Environment (commonJSHash, production, emojiJSHash, imCSSHash, imJSHash)
import Shared.Options.File (imageBasePath, productionBasePath)
import Shared.ContentType (ContentType(..))
import Shared.Unsafe as SU

pathery ∷ ContentType → String → String
pathery contentType file = case contentType of
      JS → jsBasePath <> fileName file <> ".bundle.js"
      CSS → cssBasePath <> fileName file <> ".css"
      PNG → imageBasePath <> file <> ".png"
      _ → file

jsBasePath ∷ String
jsBasePath = if production then productionBasePath else  "/client/javascript/"

cssBasePath ∷ String
cssBasePath = if production then productionBasePath else  "/client/css/"

fileName ∷ String → String
fileName file
      | production = file
      | otherwise = SU.fromJust <<< DA.head $ DS.split (Pattern ".") file

updateHash ∷ String
updateHash = commonJSHash <> imJSHash <> emojiJSHash <> imCSSHash