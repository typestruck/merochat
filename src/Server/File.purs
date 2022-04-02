module Server.File where

import Prelude

import Data.Foldable as FD
import Data.HashMap as DH
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Environment (development)
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFS
import Run as R
import Server.Response as SR
import Server.Types (BaseEffect, Configuration)
import Shared.Options.File (allowedMediaTypes, maxImageSize, maxImageSizeKB)
import Shared.Unsafe as SU

foreign import init_ ∷ EffectFn2 String String Unit

foreign import upload_ ∷ EffectFn2 String Buffer Unit

type AuthorizeAccountResponse =
      { authorizationToken ∷ String
      , apiUrl ∷ String
      }

type GetUploadUrlResponse =
      { authorizationToken ∷ String
      , uploadUrl ∷ String
      }

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
                  if FD.any (_ == mediaType) $ DH.keys allowedMediaTypes then do
                        buffer ← R.liftEffect $ NB.fromString base64 Base64
                        bufferSize ← R.liftEffect $ NB.size buffer
                        if bufferSize > maxImageSize then
                              SR.throwBadRequest imageTooBigMessage
                        else do
                              uuid ← R.liftEffect (DU.toString <$> DU.genUUID)
                              let fileName = uuid <> SU.fromJust (DH.lookup mediaType allowedMediaTypes)
                              if development then do
                                    let localPath = "src/Client/media/upload/"
                                    exists ← R.liftAff $ NFS.exists localPath
                                    unless exists <<< R.liftAff $ NFS.mkdir localPath
                                    R.liftAff $ NFS.writeFile (localPath <> fileName) buffer
                              else do
                                    liftEffect $ upload fileName buffer
                              pure fileName
                  else
                        invalidImage
            _ → invalidImage
      where
      invalidImage = SR.throwBadRequest invalidImageMessage