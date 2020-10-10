module Client.Common.Network where

import Client.Common.Types
import Prelude

import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Control.Monad.Error.Class as CMEC
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe as DM
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Payload.Client (ClientError(..), ClientResponse, defaultOpts)
import Payload.Client as PC
import Payload.ResponseTypes (Response(..))
import Shared.Spec (spec)
import Shared.Types
import Web.DOM.Element as WDE

request :: _
request = PC.mkGuardedClient (defaultOpts { baseUrl = "http://localhost:8000/" }) spec

-- | Performs a request that has loading UI and possible error/success messages
formRequest :: forall a. String -> Aff (ClientResponse a) -> Aff RequestStatus
formRequest formSelector aff = do
      previousLabel <- liftEffect do
            button <- CCD.unsafeQuerySelector buttonSelector
            CCD.value button
      setLoading (loadingMessage previousLabel) ""
      result <- aff
      case result of
            Right _ -> do
                  setLoading previousLabel ""
                  notifySuccess
                  pure Success
            Left err -> do
                  setLoading previousLabel $ errorMessage err
                  pure Fail
      where formSelectorID = if DS.take 1 formSelector == "."  then formSelector else  "#" <> formSelector
            buttonSelector = formSelectorID <> " input[type=button], " <> formSelectorID <> " button"
            errorMessageSelector = formSelectorID <> " .request-error-message"

            loadingMessage baseText =
                  let split = DS.split (Pattern " ") baseText
                  in DS.joinWith " " <<< DM.fromMaybe [] $ DA.modifyAt 0 continuous split

            continuous word =
                  let   suffix = "ing"
                        {before, after} = DS.splitAt (DS.length word - 1) word
                  in
                        if after == "e" then
                              before <> suffix
                         else if DS.length before == 2 && isVowel (DS.drop 1 before) then
                              before <> after <> after <> suffix
                         else
                              word <> suffix

            isVowel letter = DA.elem letter ["a", "e", "i", "o", "u"]

            setLoading buttonText errorText = liftEffect do
                  button <- liftEffect $ CCD.unsafeQuerySelector buttonSelector
                  errorElement <- liftEffect $ CCD.unsafeQuerySelector errorMessageSelector
                  CCD.toggleDisabled button
                  CCD.setValue button buttonText
                  CCD.setInnerHTML errorElement errorText

            notifySuccess = liftEffect do
                  formDiv <- CCD.unsafeQuerySelector formSelectorID
                  WDE.setClassName "input success" formDiv

-- | Performs a request that has can be retried through the UI in case of errors
retryableResponse :: forall response. RetryableRequest -> (response -> IMMessage) -> Aff (ClientResponse response) -> Aff (Maybe IMMessage)
retryableResponse requestMessage message aff = do
      result <- aff
      case result of
            Right r -> pure <<< Just <<< message <<< _.body $ DN.unwrap r
            Left err -> do
                  liftEffect <<< EC.log $ "Response error: " <> show err
                  pure <<< Just $ RequestFailed { request: requestMessage, errorMessage : errorMessage err }

-- | Perform a request, throwing on errors
silentResponse :: forall a. Aff (ClientResponse a) -> Aff a
silentResponse aff = do
      result <- aff
      case result of
            Right r -> pure <<< _.body $ DN.unwrap r
            Left err -> CMEC.throwError <<< EE.error $ "Response error: " <> show err

-- | this will be removed in favor of better ui than alert for errors
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

