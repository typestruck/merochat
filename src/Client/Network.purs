module Client.Network where

import Prelude
import Shared.Im.Types

import Client.Dom as CCD
import Control.Monad.Error.Class as CMEC
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Payload.Client (ClientError, ClientResponse, defaultOpts)
import Payload.Client as PC
import Payload.Headers (Headers)
import Payload.ResponseTypes (Response(..), HttpStatus)
import Safe.Coerce (class Coercible)
import Safe.Coerce as SC
import Shared.Network (RequestStatus(..))
import Shared.Network as SN
import Shared.Spec (spec)
import Web.DOM.Element as WDE

request ∷ _
request = PC.mkGuardedClient (defaultOpts { baseUrl = "/" }) spec

-- | Performs a request that has loading UI and possible error/success messages
formRequest ∷ ∀ a. String → Aff (ClientResponse a) → Aff RequestStatus
formRequest formSelector aff = do
      previousLabel ← liftEffect do
            button ← CCD.unsafeQuerySelector buttonSelector
            CCD.value button
      setLoading (loadingMessage previousLabel) ""
      result ← aff
      case result of
            Right _ → do
                  setLoading previousLabel ""
                  notifySuccess
                  pure Success
            Left err → do
                  let errorMessage = SN.errorMessage err
                  setLoading previousLabel errorMessage
                  pure $ Failure errorMessage
      where
      formSelectorId = if DS.take 1 formSelector == "." then formSelector else "#" <> formSelector
      buttonSelector = formSelectorId <> " input[type=button], " <> formSelectorId <> " button"
      errorMessageSelector = formSelectorId <> " .request-error-message"

      loadingMessage baseText = DS.joinWith " " <<< DM.fromMaybe [] <<< DA.modifyAt 0 continuous $ DS.split (Pattern " ") baseText

      continuous word =
            let
                  suffix = "ing"
                  { before, after } = DS.splitAt (DS.length word - 1) word
            in
                  if after == "e" then
                        before <> suffix
                  else if DS.length before == 2 && isVowel (DS.drop 1 before) then
                        before <> after <> after <> suffix
                  else
                        word <> suffix

      isVowel letter = DA.elem letter [ "a", "e", "i", "o", "u" ]

      setLoading buttonText errorText = liftEffect do
            button ← liftEffect $ CCD.unsafeQuerySelector buttonSelector
            errorElement ← liftEffect $ CCD.unsafeQuerySelector errorMessageSelector
            CCD.toggleDisabled button
            CCD.setValue button buttonText
            CCD.setInnerHTML errorElement errorText

      notifySuccess = liftEffect do
            formDiv ← CCD.unsafeQuerySelector formSelectorId
            existingClasses ← WDE.className formDiv
            WDE.setClassName (existingClasses <> " input success") formDiv

-- | Performs a request that has can be retried through the UI in case of errors
retryableResponse ∷ ∀ response. RetryableRequest → (response → ImMessage) → Aff (ClientResponse response) → Aff (Maybe ImMessage)
retryableResponse requestMessage message aff = do
      result ← aff
      case result of
            Right r → pure <<< Just $ message (SC.coerce r ∷ { body ∷ response, status ∷ HttpStatus, headers ∷ Headers }).body
            Left err → do
                  logError err
                  pure <<< Just $ RequestFailed { request: requestMessage, errorMessage: Just $ SN.errorMessage err }

-- | Perform a request, throwing on errors
silentResponse ∷ ∀ a. Aff (ClientResponse a) → Aff a
silentResponse aff = do
      result ← aff
      case result of
            Right r → pure (SC.coerce r ∷ { body ∷ a, status ∷ HttpStatus, headers ∷ Headers }).body
            Left err → CMEC.throwError <<< EE.error $ "Response error: " <> show err

defaultResponse ∷ ∀ r. Aff (ClientResponse r) → Aff (Either ClientError r)
defaultResponse aff = map (_.body <<< coerce) <$> aff
      where
      coerce :: Response r -> { body ∷ r, status ∷ HttpStatus, headers ∷ Headers }
      coerce = SC.coerce

logError ∷ ∀ e. Show e ⇒ e → Aff Unit
logError err = liftEffect <<< EC.log $ "Response error: " <> show err
