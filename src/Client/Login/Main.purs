-- this needs the header and footer of external pages

module Client.Login.Main where

import Prelude

import Client.AppId (loginAppId)
import Client.Location as CL
import Client.Network (routes)
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
import Shared.Login.Types (LoginModel, LoginMessage(..))
import Shared.Login.View as SLV
import Shared.Network as SN
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Routes (routesSpec)

initialModel ∷ LoginModel
initialModel = { email: "", password: "", loading: false, error: "" }

application ∷ Effect Unit → Application LoginModel LoginMessage
application authenticated =
      { model: initialModel
      , view: SLV.view
      , update: update authenticated
      , subscribe: []
      }

update ∷ Effect Unit → Update LoginModel LoginMessage
update authenticated model message = case message of
      LoggedIn → loggedIn model
      SetEmail email → setEmail email model
      SetPassword password → setPassword password model
      Failed error → failed error model
      KeyPressed key → keyPressed authenticated key model
      Submit → submit authenticated model

loggedIn ∷ LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
loggedIn _ = F.noMessages initialModel

setEmail ∷ String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
setEmail email model = F.noMessages $ model { email = email }

setPassword ∷ String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
setPassword password model = F.noMessages $ model { password = password }

failed ∷ String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
failed error model = F.noMessages $ model { loading = false, error = error }

keyPressed ∷ Effect Unit → String → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
keyPressed authenticated key model = case key of
      "Enter" → submit authenticated model
      _ → F.noMessages model

submit ∷ Effect Unit → LoginModel → LoginModel /\ Array (Aff (Maybe LoginMessage))
submit authenticated model
      | model.loading = F.noMessages model
      | otherwise =
              let
                    email = DS.trim model.email
              in
                    if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then
                          F.noMessages $ model { error = "Please enter a valid email" }
                    else if DS.length model.password < passwordMinCharacters then
                          F.noMessages $ model { error = "Password must be " <> show passwordMinCharacters <> " characters or more" }
                    else model { loading = true, error = "" } /\
                          [ do
                                  result ← EA.attempt $ routes.login.post { body: { email, password: model.password } }
                                  case result of
                                        Left error → pure <<< Just <<< Failed $ EE.message error
                                        Right (Left error) → pure <<< Just <<< Failed $ SN.errorMessage error
                                        Right (Right _) → EC.liftEffect authenticated *> pure (Just LoggedIn)
                          ]

main ∷ Effect Unit
main = F.mount loginAppId $ application do
      redirect ← CL.queryParameter "next"
      CL.setLocation $ DM.fromMaybe (routesSpec.im.get {}) redirect
