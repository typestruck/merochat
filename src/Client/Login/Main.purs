module Client.Login.Main where

import Prelude

import Client.Common as C
import Client.Common.External as E
import Data.Either as DET
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.String as S
import Effect (Effect)
import Effect.Aff as A
import Effect.Class (liftEffect)
import Shared.Routing as R
import Shared.Types (RegisterLogin(..), Route(..), Token(..))
import Type.Data.Boolean (kind Boolean)
import Web.UIEvent.MouseEvent.EventTypes (click)

login :: Effect Unit
login = do
	registerLogin  <- E.validateEmailPassword
	M.maybe (pure unit) $ \rl -> A.launchAff_ $ C.post' (R.toResource Login) rl enter
	where  -- the location to go after login is either the query parameter next or /im
		defaultNext = R.fromRouteAbsolute $ Login {next: Just $ R.fromRouteAbsolute IM }

		next [] = defaultNext
		next location = R.fromRouteAbsolute $ Login {next: Just location}

		enter token = liftEffect $ do
			redirect <- R.toRoute <<< next <$> C.search
			E.login token $ DET.either defaultNext identity redirect

main :: Effect Unit
main = do
	loginButton <- C.querySelector "#login"
	C.addEventListener loginButton click (const login)
