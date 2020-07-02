module Client.IM.UserMenu where

import Prelude
import Shared.IM.Types

import Client.Common.DOM as CCD
import Client.Common.Logout as CCLO
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages)
import Client.IM.Flame as CIF
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Types (JSONResponse(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

update :: IMModel -> UserMenuMessage -> MoreMessages
update model =
        case _ of
                ConfirmLogout -> confirmLogout model
                Logout confirmed -> logout model confirmed
                ShowUserContextMenu event -> showUserContextMenu model event
                ToggleProfileSettings toggle -> toggleProfileSettings model toggle
                SetModalContents file root (JSONResponse html) -> CIF.nothingNext model $ loadModal root html file
                SetUserContentMenuVisible toggle -> F.noMessages $ SN.updateModel model $ _ {  userContextMenuVisible = toggle }

logout :: IMModel -> Boolean -> MoreMessages
logout model confirmed = CIF.nothingNext model <<< liftEffect $ when confirmed CCLO.logout

confirmLogout :: IMModel -> NoMessages
confirmLogout = (_ :> [Just <<< UMM <<< Logout <$> liftEffect (CCD.confirm "Really log out?")])

--PERFORMANCE: load bundles only once
toggleProfileSettings :: IMModel -> ProfileSettingsToggle -> MoreMessages
toggleProfileSettings model =
        case _ of
                ShowProfile -> showTab Profile ShowProfile "profile.bundle.js" "#profile-edition-root"
                ShowSettings -> showTab Settings ShowSettings "settings.bundle.js" "#settings-edition-root"
                Hidden -> CIF.justNext (SN.updateModel model $ _ { profileSettingsToggle = Hidden }) <<< UMM <<< SetModalContents Nothing "#profile-edition-root" $ JSONResponse "Loading..."
        where   showTab route toggle file root =
                        (SN.updateModel model $ _ { profileSettingsToggle = toggle }) :> [
                                Just <<< UMM <<< SetModalContents (Just file) root <$> CCN.get' route
                        ]
loadModal :: String -> String -> Maybe String -> Aff Unit
loadModal root html file = liftEffect do
        element <- CCD.querySelector root
        CCD.setInnerHTML element html
        --scripts don't load when inserted via innerHTML
        case file of
                Just name -> CCD.loadScript name
                Nothing -> pure unit

showUserContextMenu :: IMModel -> Event -> MoreMessages
showUserContextMenu model@(IMModel { userContextMenuVisible }) event
        | userContextMenuVisible =
                F.noMessages <<< SN.updateModel model $ _ { userContextMenuVisible = false }
        | otherwise =
                model :> [
                        liftEffect <<< map (Just <<< UMM <<< SetUserContentMenuVisible <<< (_ == "user-context-menu")) $ WDE.id <<< SU.unsafeFromJust "usermenu.update" $ do
                        target <- WEE.target event
                        WDE.fromEventTarget target
                ]
