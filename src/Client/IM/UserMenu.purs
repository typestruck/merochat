module Client.IM.UserMenu where

import Prelude
import Shared.IM.Types

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (World)
import Shared.Cookies (cookieName)
import Shared.Cookies as SC
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

update :: World IMModel IMMessage -> IMModel -> UserMenuMessage -> Aff IMModel
update world model =
        case _ of
                Logout -> logout model
                ShowUserContextMenu event -> showUserContextMenu model event
                ShowProfile -> showProfile world model

showProfile :: World IMModel IMMessage -> IMModel -> Aff IMModel
showProfile world model = do
        let updatedModel = SN.updateModel model $ _ {
                profileEditionVisible = true
        }
        world.view updatedModel
        pure updatedModel


showUserContextMenu :: IMModel -> Event -> Aff IMModel
showUserContextMenu model@(IMModel { userContextMenuVisible }) event = do
        shouldBeVisible <-
                if userContextMenuVisible then
                        pure false
                 else
                        liftEffect <<< map ( _ == "user-context-menu") $ WDE.id <<< SU.unsafeFromJust "usermenu.update" $
                                do
                                target <- WEE.target event
                                WDE.fromEventTarget target
        pure <<< SN.updateModel model $ _ {
                userContextMenuVisible = shouldBeVisible
        }

logout :: IMModel -> Aff IMModel
logout model = do
        confirmed <- liftEffect $ CCD.confirm "Really log out?"
        when confirmed $ void do
                liftEffect do
                        CCS.removeItem tokenKey
                        SC.removeCookie cookieName
                        CCL.setLocation "/"
        pure model
