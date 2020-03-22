module Client.Login.Main where

import Prelude
import Shared.Types

import Client.Common as CC
import Client.Common.External as CCE
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Debug.Trace(spy)
import Effect.Class (liftEffect)
import Shared.Routing as SR
import Shared.Unsafe as SU
import Web.UIEvent.MouseEvent.EventTypes (click)
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.KeyboardEvent as WUK

login :: Effect Unit
login = do
        maybeRegisterLogin  <- CCE.validateEmailPassword
        case maybeRegisterLogin of
                Nothing -> pure unit
                Just registerLogin -> EA.launchAff_ $ do
                        token <- CC.post' (SR.fromRouteAbsolute $ Login { next: Nothing }) registerLogin
                        liftEffect $ do
                                -- the location to go after login is either the query parameter next or /im
                                redirect <- SR.toRoute <$> CC.search
                                CCE.login token $ DE.either (const defaultNext) SR.fromRouteAbsolute redirect
        where   defaultNext = SR.fromRouteAbsolute IM


loginOnEnter event = do
        let pressed = WUK.key <<< SU.unsafeFromJust "registerOnEnter" $ WUK.fromEvent event
        when (pressed == "Enter") login

main :: Effect Unit
main = do
        loginButton <- CC.querySelector "#login"
        signUpDiv <- CC.querySelector ".box-action"
        CC.addEventListener signUpDiv keyup loginOnEnter
        CC.addEventListener loginButton click (const login)
