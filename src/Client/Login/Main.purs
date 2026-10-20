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
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Flame (Application, Update)
import Flame as F
import Shared.Login.View as SLV
import Shared.Network as SN
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Routes (routesSpec)

type Model = { email ∷ String, password ∷ String, loading ∷ Boolean, error ∷ String }

data Message = SetEmail String | SetPassword String | Submit | KeyPressed String | Failed String | LoggedIn

initialModel ∷ Model
initialModel = { email: "", password: "", loading: false, error: "" }

application ∷ Effect Unit → Application Model Message
application authenticated =
      { model: initialModel
      , view: SLV.view { setEmail: SetEmail, setPassword: SetPassword, submit: Submit, keyPressed: KeyPressed }
      , update: update authenticated
      , subscribe: []
      }

update ∷ Effect Unit → Update Model Message
update authenticated model message = case message of
      LoggedIn → F.noMessages initialModel
      SetEmail email → F.noMessages $ model { email = email }
      SetPassword password → F.noMessages $ model { password = password }
      Failed error → F.noMessages $ model { loading = false, error = error }
      KeyPressed "Enter" → update authenticated model Submit
      KeyPressed _ → F.noMessages model
      Submit | model.loading → F.noMessages model
      Submit →
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
