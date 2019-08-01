module Client.Login.Main where

import Prelude

import Client.Common as CC
import Client.Common.External as CCE
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Routing as SR
import Shared.Types
import Web.UIEvent.MouseEvent.EventTypes (click)

login :: Effect Unit
login = do
	registerLogin  <- CCE.validateEmailPassword
	case registerLogin of
		Nothing -> pure unit
		Just rl -> EA.launchAff_ $ do
			token <- CC.post' (SR.fromRouteAbsolute $ Login { next: Nothing }) rl
			liftEffect $ do
				redirect <- SR.toRoute <<< next <$> CC.search
				CCE.login token $ DE.either (const defaultNext) SR.fromRouteAbsolute redirect
	where  -- the location to go after login is either the query parameter next or /im
		defaultNext = SR.fromRouteAbsolute $ Login { next: Just $ SR.fromRouteAbsolute IM }

		next "" = defaultNext
		next location = SR.fromRouteAbsolute $ Login { next: Just location }

main :: Effect Unit
main = do
	loginButton <- CC.querySelector "#login"
	CC.addEventListener loginButton click (const login)
