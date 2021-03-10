module Server.File where

import Prelude
import Server.Types

import Affjax as A
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestBody as ARB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut as DAD
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Int8Array)
import Data.Either (Either(..))
import Data.Foldable as FD
import Data.HTTP.Method (Method(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.UUID as DU
import Effect.Class.Console as EC
import Effect.Ref (Ref)
import Effect.Ref as ER
import Environment (development)
import Foreign.Object as FO
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Node.Crypto.Hash (Algorithm(..))
import Node.Crypto.Hash as NCH
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFS
import Run as R
import Run.Reader as RR
import Server.Response as SR
import Shared.Options.File (allowedMediaTypes, imageBasePath, maxImageSize, maxImageSizeKB)
import Shared.Unsafe as SU
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Coerce as UC
import Web.File.Blob (Blob)

foreign import sha1 :: Buffer -> String

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for pictures is " <> maxImageSizeKB

-- use the fucking library
saveBase64File :: forall r. String -> BaseEffect { storageDetails :: Ref StorageDetails | r } String
saveBase64File input =
      case DS.split (Pattern ",") input of
            [mediaType, base64] -> do
                  if FD.any (_ == mediaType) $ DH.keys allowedMediaTypes then do
                        buffer <- R.liftEffect $ NB.fromString base64 Base64
                        bufferSize <- R.liftEffect $ NB.size buffer
                        if bufferSize > maxImageSize then
                                    SR.throwBadRequest imageTooBigMessage
                              else do
                                    uuid <- R.liftEffect (DU.toString <$> DU.genUUID)
                                    let fileName = uuid <> SU.fromJust (DH.lookup mediaType allowedMediaTypes)
                                    if development then
                                          R.liftAff $ NFS.writeFile ("src/Client/media/upload/" <> fileName) buffer
                                    else do
                                          { storageDetails } <- RR.ask
                                          getAuthorizationToken storageDetails
                                          getUploadUrl storageDetails
                                          uploadFile storageDetails fileName buffer

                                    pure fileName
                  else
                        invalidImage
            _ -> invalidImage
      where invalidImage = SR.throwBadRequest invalidImageMessage
            bucketId = "14b84c0cac42e8747562021d"

            getAuthorizationToken :: Ref StorageDetails -> _
            getAuthorizationToken storageDetails = do
                  { authenticationKey, accountAuthorizationToken, apiUrl } <- R.liftEffect $ ER.read storageDetails
                  case accountAuthorizationToken, apiUrl of
                        Just _, Just _ -> pure unit
                        _, _ -> do
                              response <- R.liftAff <<< A.request $ A.defaultRequest {
                                    url = "https://api.backblazeb2.com/b2api/v2/b2_authorize_account",
                                    method = Left GET,
                                    responseFormat = RF.json,
                                    headers = [RequestHeader "Authorization" $ "Basic " <> authenticationKey ]
                              }
                              case response of
                                    Right { body, statusText } -> do
                                          let authorization = SU.fromRight (DAD.decodeJson body) :: AuthorizeAccountResponse
                                          R.liftEffect $ ER.modify_ ( _ { apiUrl = Just authorization.apiUrl, accountAuthorizationToken = Just authorization.authorizationToken}) storageDetails
                                    Left left -> SR.throwInternalError $ A.printError left

            getUploadUrl :: Ref StorageDetails -> _
            getUploadUrl storageDetails = do
                  { accountAuthorizationToken, apiUrl, uploadAuthorizationToken, uploadUrl } <- R.liftEffect $ ER.read storageDetails
                  case uploadAuthorizationToken, uploadUrl of
                        Just t, Just u -> pure unit
                        _, _ -> do
                              response <- R.liftAff <<< A.request $ A.defaultRequest {
                                    url = SU.fromJust apiUrl <> "/b2api/v2/b2_get_upload_url",
                                    method = Left POST,
                                    responseFormat = RF.json,
                                    headers = [RequestHeader "Authorization" $ SU.fromJust accountAuthorizationToken],
                                    content = Just <<< Json $ DAD.encodeJson { bucketId }
                              }
                              case response of
                                    Right { status, body, statusText } -> do
                                          if status == StatusCode 200 then do
                                                let upload = SU.fromRight (DAD.decodeJson body) :: GetUploadUrlResponse
                                                R.liftEffect $ ER.modify_ ( _ { uploadAuthorizationToken = Just upload.authorizationToken, uploadUrl = Just upload.uploadUrl }) storageDetails
                                           else if status == StatusCode 401 then do
                                                R.liftEffect $ ER.modify_ ( _ { uploadAuthorizationToken = Nothing, uploadUrl = Nothing, accountAuthorizationToken = Nothing, apiUrl = Nothing }) storageDetails

                                                void $ saveBase64File input
                                                pure unit
                                           else
                                                SR.throwInternalError statusText
                                    Left left -> SR.throwInternalError $ A.printError left

            uploadFile :: Ref StorageDetails -> String -> Buffer -> _
            uploadFile storageDetails fileName buffer = do
                  { uploadAuthorizationToken, uploadUrl } <- R.liftEffect $ ER.read storageDetails
                  response <- R.liftAff <<< A.request $ A.defaultRequest {
                        url = SU.fromJust uploadUrl,
                        method = Left POST,
                        responseFormat = RF.json,
                        headers = [
                              RequestHeader "Authorization" $ SU.fromJust uploadAuthorizationToken,
                              RequestHeader "X-Bz-File-Name" $ "upload/" <> fileName,
                              RequestHeader "content-type" "b2/x-auto",
                              RequestHeader "X-Bz-Content-Sha1" $ sha1 buffer
                        ],
                        content = Just $ ARB.arrayView (UC.unsafeCoerce buffer)
                  }
                  case response of
                        Right { status, body, statusText } -> do
                              if status == StatusCode 200 then do
                                    pure unit
                               else if status == StatusCode 401 then do
                                    R.liftEffect $ ER.modify_ ( _ { uploadAuthorizationToken = Nothing, uploadUrl = Nothing }) storageDetails

                                    void $ saveBase64File input
                                    pure unit
                               else
                                    SR.throwInternalError statusText
                        Left left -> SR.throwInternalError $ A.printError left
