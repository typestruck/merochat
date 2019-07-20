module Client.Login.Main where

import Prelude

import Client.Common as CC
import Client.Common.External as CCE
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Routing as SR
import Shared.Types (RegisterLogin(..), Route(..), Token(..))
import Web.UIEvent.MouseEvent.EventTypes (click)

login :: Effect Unit
login = do
	registerLogin  <- CCE.validateEmailPassword
	EA.launchAff_ $
		pure unit-- <- CC.post' (SR.toRoute Login) rl enter
	-- where  -- the location to go after login is either the query parameter next or /im
	-- 	defaultNext = SR.fromRouteAbsolute $ Login {next: Just $ SR.fromRouteAbsolute IM }

	-- 	next "" = defaultNext
	-- 	next location = SR.fromRouteAbsolute $ Login {next: Just location}

	-- 	enter token = liftEffect $ do
	-- 		redirect <- SR.toRoute <<< next <$> CC.search
	-- 		CCE.login token $ DE.either (const defaultNext) SR.fromRouteAbsolute redirect

main :: Effect Unit
main = do
	loginButton <- CC.querySelector "#login"
	CC.addEventListener loginButton click (const login)
