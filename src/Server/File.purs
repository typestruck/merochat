module Server.File where

import Prelude

import Data.Foldable as FD
import Data.HashMap as DH
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Environment (production)
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Effect.Aff(Aff)
import Node.FS.Sync as NFS
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFA
import Data.Maybe(Maybe(..))
import Run as R
import Data.Set as DS
import Server.Response as SR
import Server.Effect (BaseEffect, Configuration)
import Shared.Options.File
import Shared.Unsafe as SU
import Control.Promise (Promise)
import Control.Promise as CP

foreign import init_ ∷ EffectFn2 String String Unit
foreign import upload_ ∷ EffectFn2 String Buffer Unit

foreign import realFileExtension_ ∷ Buffer -> Effect (Promise String)

realFileExtension :: Buffer -> Aff String
realFileExtension buffer = CP.toAffE $ realFileExtension_ buffer

upload ∷ String → Buffer → Effect Unit
upload = EU.runEffectFn2 upload_

init ∷ String → String → Effect Unit
init = EU.runEffectFn2 init_

invalidImageMessage ∷ String
invalidImageMessage = "Invalid image"

imageTooBigMessage ∷ String
imageTooBigMessage = "Max allowed size for pictures is " <> maxImageSizeKB

saveBase64File ∷ ∀ r. String → BaseEffect { configuration ∷ Configuration | r } String
saveBase64File input =
      case DS.split (Pattern ",") input of
            [ mediaType, base64 ] → do
                  case DH.lookup mediaType allowedMediaTypes of
                        Nothing -> invalidImage
                        Just _ -> do
                              buffer ← R.liftEffect $ NB.fromString base64 Base64
                              bufferSize ← R.liftEffect $ NB.size buffer
                              if bufferSize > maxImageSize then
                                    SR.throwBadRequest imageTooBigMessage
                              else do
                                    extension <- map ("." <> _) <<< R.liftAff $ realFileExtension buffer
                                    if DS.member extension allowedExtensions then do
                                          uuid ← R.liftEffect (DU.toString <$> DU.genUUID)
                                          let fileName = uuid <> extension
                                          if production then
                                                liftEffect $ upload fileName buffer
                                          else do
                                                let localPath = "src/Client/media/upload/"
                                                exists ← R.liftEffect $ NFS.exists localPath
                                                unless exists <<< R.liftAff $ NFA.mkdir localPath
                                                R.liftAff $ NFA.writeFile (localPath <> fileName) buffer
                                          pure fileName
                                    else
                                          invalidImage
            _ → invalidImage
      where
      invalidImage = SR.throwBadRequest invalidImageMessage