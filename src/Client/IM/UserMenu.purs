module Client.IM.UserMenu where

import Prelude
import Shared.IM.Types

import Client.Common.Cookies as CCC
import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate, Environment)
import Flame.Application.Effectful as FAE
import Shared.Router as SR
import Shared.Types (JSONString(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

update :: AffUpdate IMModel UserMenuMessage
update environment@{ model, message } =
        case message of
                Logout -> do
                        liftEffect logout
                        FAE.noChanges
                ShowUserContextMenu event -> FAE.diff' <$> showUserContextMenu model event
                ToggleProfileSettings toggle -> toggleProfileSettings environment toggle

--PERFORMANCE: load bundles only once
toggleProfileSettings :: Environment IMModel UserMenuMessage -> ProfileSettingsToggle -> Aff (IMModel -> IMModel)
toggleProfileSettings { display } =
        case _ of
                ShowProfile -> showTab Profile ShowProfile "profile.bundle.js" "#profile-edition-root"
                ShowSettings -> showTab Settings ShowSettings "settings.bundle.js" "#settings-edition-root"
                Hidden -> do
                        setRootHTML "Loading..." "#profile-edition-root"
                        FAE.diff { profileSettingsToggle: Hidden }

        where   showTab route toggle file root = do
                        display $ FAE.diff' { profileSettingsToggle: toggle }
                        JSONString html <- CCN.get' $ SR.fromRouteAbsolute route
                        setRootHTML html root
                        --scripts don't load when inserted via innerHTML
                        liftEffect $ CCD.loadScript file
                        FAE.noChanges

                setRootHTML html root = liftEffect do
                        element <- CCD.querySelector root
                        CCD.setInnerHTML element html

showUserContextMenu :: IMModel -> Event -> Aff { userContextMenuVisible :: Boolean }
showUserContextMenu model@(IMModel { userContextMenuVisible }) event = do
        shouldBeVisible <-
                if userContextMenuVisible then
                        pure false
                 else
                        liftEffect <<< map ( _ == "user-context-menu") $ WDE.id <<< SU.unsafeFromJust "usermenu.update" $ do
                                target <- WEE.target event
                                WDE.fromEventTarget target
        pure {
                userContextMenuVisible: shouldBeVisible
        }

logout :: Effect Unit
logout = do
        confirmed <- CCD.confirm "Really log out?"
        when confirmed $ void do
                CCS.removeItem tokenKey
                CCC.removeMelanchatCookie
                CCL.setLocation "/"
