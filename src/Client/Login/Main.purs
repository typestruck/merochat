-- this needs the header and footer of external pages

module Client.Login.Main where

import Prelude

import Client.AppId (imAppId, loginAppId)
import Client.Location as CL
import Client.Lynx.Room as CLR
import Client.Network (routes)
import Client.Network as CN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Flame (Application, Update)
import Flame as F
import Payload.Client (ClientError(..))
import Shared.Account (EmailPassword)
import Shared.Login.Types (LoginModel, LoginMessage(..), LoginResponse)
import Shared.Login.View as SLV
import Shared.Network as SN
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Routes (routesSpec)

initialModel ∷ LoginModel
initialModel = { email: Nothing, password: Nothing, loading: false, error: Nothing }

application ∷ (EmailPassword → Aff Unit) → Application LoginModel LoginMessage
application authenticate =
      { model: initialModel
      , view: SLV.view
      , update: update authenticate
      , subscribe: []
      }

update ∷ (EmailPassword → Aff Unit) → Update LoginModel LoginMessage
update authenticate model message = case message of
      Reset → reset model
      SetEmail email → setEmail email model
      SetPassword password → setPassword password model
      Failed error → failed error model
      KeyPressed key → keyPressed authenticate key model
      Submit → submit authenticate model

reset ∷ LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
reset _ = F.noMessages initialModel

setEmail ∷ Maybe String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
setEmail email model = F.noMessages $ model { email = email }

setPassword ∷ Maybe String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
setPassword password model = F.noMessages $ model { password = password }

failed ∷ String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
failed error model = F.noMessages $ model { loading = false, error = Just error }

keyPressed ∷ (EmailPassword → Aff Unit) → String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
keyPressed authenticate key model = case key of
      "Enter" → submit authenticate model
      _ → F.noMessages model

submit ∷ (EmailPassword → Aff Unit) → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
submit authenticate model
      | model.loading = F.noMessages model
      | otherwise =
              let
                    email = DS.trim $ DM.fromMaybe "" model.email
                    password = DM.fromMaybe "" model.password
              in
                    if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then
                          F.noMessages $ model { error = Just "Please enter a valid email" }
                    else if DS.length password < passwordMinCharacters then
                          F.noMessages $ model { error = Just $ "Password must be " <> show passwordMinCharacters <> " characters or more" }
                    else model { loading = true, error = Nothing } /\
                          [ do
                                  result ← EA.attempt $ authenticate { email, password }
                                  pure <<< Just $ case result of
                                        Left error → Failed $ EE.message error
                                        Right _ → Reset
                          ]

login ∷ Boolean → EmailPassword → Aff LoginResponse
login boot credentials = do
      result ← CN.request $ routes.login.post { body: credentials, query: { boot } }
      case result of
            Left (DecodeError { error }) → EA.throwError <<< EE.error $ "Server sent an unexpected login response: " <> show error
            Left error → EA.throwError <<< EE.error $ SN.errorMessage error
            Right payload → pure payload

authenticateNative ∷ EmailPassword → Aff Unit
authenticateNative credentials = do
      payload ← login true credentials
      case payload.user of
            Nothing → EA.throwError $ EE.error "Login response is missing the current user"
            Just user → do
                  CLR.insert user
                  CLR.insert payload.contacts
                  EC.liftEffect $ F.navigate imAppId

main ∷ Effect Unit
main = F.mount loginAppId $ application \credentials → do
      void $ login false credentials
      EC.liftEffect do
            redirect ← CL.queryParameter "next"
            CL.setLocation $ DM.fromMaybe (routesSpec.im.get {}) redirect
