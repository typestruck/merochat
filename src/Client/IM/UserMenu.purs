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
                ToggleProfile isVisible -> showProfile environment isVisible

showProfile :: Environment IMModel UserMenuMessage -> Boolean -> Aff (IMModel -> IMModel)
showProfile { display, model } =
        case _ of
                true -> do
                        display $ FAE.diff' { profileEditionVisible: true }
                        JSONString html <- CCN.get' $ SR.fromRouteAbsolute Profile
                        setRootHTML html
                        FAE.noChanges
                false -> do
                        setRootHTML "Loading..."
                        FAE.diff { profileEditionVisible: false }

        where   setRootHTML html = liftEffect do
                        element <- CCD.querySelector "#profile-edition-root"
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
