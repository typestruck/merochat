module Client.Login.Main where

import Prelude
import Shared.Types

import Client.Common.Network as CCNT
import Client.Common.Location as CCL
import Client.Common.DOM as CCD
import Client.Common.External as CCE
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Debug.Trace(spy)
import Effect.Class (liftEffect)
import Shared.Router as SR
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
                        token <- CCNT.post' (SR.fromRoute $ Login { next: Nothing }) $ Just registerLogin
                        liftEffect $ do
                                -- the location to go after login is either the query parameter next or /im
                                redirect <- SR.toRoute <$> CCL.search
                                CCE.login token $ DE.either (const defaultNext) SR.fromRoute redirect
        where   defaultNext = SR.fromRoute IM


loginOnEnter event = do
        let pressed = WUK.key <<< SU.unsafeFromJust "registerOnEnter" $ WUK.fromEvent event
        when (pressed == "Enter") login

main :: Effect Unit
main = do
        loginButton <- CCD.querySelector "#login"
        signUpDiv <- CCD.querySelector ".sign-up"
        CCD.addEventListener signUpDiv keyup loginOnEnter
        CCD.addEventListener loginButton click (const login)
