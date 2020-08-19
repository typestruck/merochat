module Client.Common.Network where

import Prelude

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Client.Common.Notification as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Control.Monad.Error.Class as CMEC
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Either (Either(..))
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype as DN
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Partial.Unsafe as PU
import Payload.Client (ClientError(..), ClientResponse)
import Payload.Client as PC
import Payload.ResponseTypes (Response(..))
import Shared.Spec (spec)
import Shared.Router as SR
import Shared.Types (Route)

-- | A simplified version of post without the option to handle errors
post' :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => Route -> Maybe contents -> Aff response
post' route data' = do
      response <- post route data'
      case response of
            Right right -> pure right
            Left error -> alertResponseError error

-- | Performs a POST request
post :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => Route -> Maybe contents -> Aff (Either String response)
post route data' = do
      --see Token in shared/Types.purs
      token <- liftEffect $ CCS.getToken
      response <- A.request $ (defaultRequest route POST token) {
            content = map (RB.json <<< DAEGR.genericEncodeJson) data'
      }
      parseBody response

get' :: forall response r. Generic response r => DecodeRep r => Route -> Aff response
get' route = do
      response <- get route
      case response of
            Right right -> pure right
            Left error -> alertResponseError error

get :: forall response r. Generic response r => DecodeRep r => Route -> Aff (Either String response)
get route = do
      token <- pure "" --liftEffect CCC.getMelanchatCookie
      response <- A.request $ defaultRequest route GET token
      parseBody response

defaultRequest route method token =
      A.defaultRequest {
            url = SR.fromRoute route,
            method = Left method,
            responseFormat = RF.json,
            headers = [
                  Accept $ MediaType "application/json",
                  ContentType $ MediaType "application/json",
                  RequestHeader "xAccessToken" token
            ]
      }

parseBody response =
      case response of
            Right { body, status } ->
                  if status == StatusCode 200 then
                        DE.either (alertResponseError <<< show) (pure <<< Right) $ DADGR.genericDecodeJson body
                   else
                        pure <<< Left <<< PU.unsafePartial $ DE.fromRight $ DAD.decodeJson body
            Left left -> pure <<< Left $ A.printError left

alertResponseError message = do
      liftEffect $ CCN.alert message
      CMEC.throwError <<< EE.error $ "Error: " <> message


---new

errorMessage :: ClientError -> String
errorMessage = case _ of
      DecodeError { response: Response { body } } -> "Could not parse response from server"
      StatusError { response: Response { body } } ->  body
      RequestError { message } -> message

request :: _
request = PC.mkGuardedClient_ spec

response :: forall a. Aff (ClientResponse a) -> Aff a
response aff = do
      result <- aff
      case result of
            Right r -> pure <<< _.body $ DN.unwrap r
            Left err -> do
                  liftEffect <<< CCN.alert $ errorMessage err
                  CMEC.throwError <<< EE.error $ "Response error: " <> show err