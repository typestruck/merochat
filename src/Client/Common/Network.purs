module Client.Common.Network where

import Prelude

import Client.Common.Notification as CCN
import Control.Monad.Error.Class as CMEC
import Data.Either (Either(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Payload.Client (ClientError(..), ClientResponse, defaultOpts)
import Payload.Client as PC
import Payload.ResponseTypes (Response(..))
import Shared.Spec (spec)

request :: _
request = PC.mkGuardedClient (defaultOpts { baseUrl = "http://localhost:8000/" }) spec

response :: forall a. Aff (ClientResponse a) -> Aff a
response aff = do
      result <- aff
      case result of
            Right r -> pure <<< _.body $ DN.unwrap r
            Left err -> do
                  liftEffect <<< CCN.alert $ errorMessage err
                  CMEC.throwError <<< EE.error $ "Response error: " <> show err

errorMessage :: ClientError -> String
errorMessage = case _ of
      DecodeError { response: Response { body } } -> "Server sent an unexpected response"
      StatusError { response: Response { body } } -> body
      RequestError { message } -> message