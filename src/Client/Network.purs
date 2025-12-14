module Client.Network
      ( request
      , formRequest
      , routes
      , retryableRequest
      , silentRequest
      ) where

import Prelude
import Shared.Im.Types (ImMessage(..), RetryableRequest)

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
import Prim.TypeError (class Warn, Text)
import Safe.Coerce as SC
import Shared.Network (RequestStatus(..))
import Shared.Network as SN
import Shared.Spec (spec)
import Web.DOM.Element as WDE

routes ∷ _
routes = PC.mkGuardedClient (defaultOpts { baseUrl = "/" }) spec

-- | Performs a request that has a loading UI and possible error/success messages
formRequest ∷ ∀ a. Warn (Text "Deprecated") ⇒ String → Aff (ClientResponse a) → Aff RequestStatus
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

-- | Performs a request that is added to the failure list in case of errors
-- |
-- | Useful for managing retry UIs
retryableRequest ∷ ∀ response. RetryableRequest → (response → ImMessage) → Aff (ClientResponse response) → Aff (Maybe ImMessage)
retryableRequest rr message aff = do
      result ← aff
      case result of
            Right r → pure <<< Just $ message (coerceResponse r).body
            Left err → do
                  logError err
                  pure <<< Just $ RequestFailed { routes: rr, errorMessage: Just $ SN.errorMessage err }

-- | Perform a request, throwing on errors
silentRequest ∷ ∀ a. Aff (ClientResponse a) → Aff a
silentRequest aff = do
      result ← aff
      case result of
            Right r → pure (coerceResponse r).body
            Left err → CMEC.throwError <<< EE.error $ "Response error: " <> show err

-- | Performs a request
request ∷ ∀ r. Aff (ClientResponse r) → Aff (Either ClientError r)
request aff = map (_.body <<< coerceResponse) <$> aff

coerceResponse ∷ ∀ r. Response r → { body ∷ r, status ∷ HttpStatus, headers ∷ Headers }
coerceResponse = SC.coerce

logError ∷ ∀ e. Show e ⇒ e → Aff Unit
logError err = liftEffect <<< EC.log $ "Response error: " <> show err
