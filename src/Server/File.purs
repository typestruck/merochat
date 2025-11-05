module Server.File where

import Prelude

import Control.Promise (Promise)
import Control.Promise as CP
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Set (member) as DST
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID as DU
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFA
import Run as R
import Server.Effect (BaseEffect)
import Server.Response as SR
import Shared.File as SF
import Shared.Resource (allowedExtensions, allowedMediaTypes, audioMediaType, localBasePath, maxImageSize, maxImageSizeKB, uploadFolder, videoMediaType)

foreign import realFileExtension_ ∷ Buffer → Effect (Promise String)

realFileExtension ∷ Buffer → Aff String
realFileExtension buffer = CP.toAffE $ realFileExtension_ buffer

foreign import isNsfw_ :: String -> Effect (Promise Boolean)

isNsfw :: String -> Aff Boolean
isNsfw filePath = CP.toAffE $ isNsfw_  filePath

invalidImageMessage ∷ String
invalidImageMessage = "Invalid file"

imageTooBigMessage ∷ String
imageTooBigMessage = "Max allowed size for files is " <> maxImageSizeKB

saveBase64File ∷ ∀ r. String → BaseEffect r String
saveBase64File input = case SF.fromBase64File input of
      Just (mediaType /\ base64) →
            case DH.lookup (DSR.replace (DSRU.unsafeRegex "\\s*?codecs=.+;" noFlags) "" mediaType) allowedMediaTypes of
                  Nothing → invalidImage
                  Just mt → do
                        buffer ← R.liftEffect $ NB.fromString base64 Base64
                        bufferSize ← R.liftEffect $ NB.size buffer
                        if bufferSize > (maxImageSize * (if mt == audioMediaType || mt == videoMediaType then 4 else 1)) then
                              SR.throwBadRequest imageTooBigMessage
                        else do
                              extension ← map ("." <> _) <<< R.liftAff $ realFileExtension buffer
                              if DST.member extension allowedExtensions then do
                                    uuid ← R.liftEffect (DU.toString <$> DU.genUUID)
                                    let fileName = uuid <> extension
                                    R.liftAff $ NFA.writeFile (localBasePath <> uploadFolder <> fileName) buffer
                                    pure fileName
                              else
                                    invalidImage
      _ → invalidImage
      where
      invalidImage = SR.throwBadRequest invalidImageMessage

