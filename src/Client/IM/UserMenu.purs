module Client.IM.UserMenu where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Routes (routes)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

logout :: IMModel -> MoreMessages
logout model = CIF.nothingNext model out
      where out = do
                  void $ request.logout { body: {} }
                  liftEffect $ CCL.setLocation $ routes.login.get {}

--PERFORMANCE: load bundles only once
toggleModal :: ShowUserMenuModal -> IMModel -> NextMessage
toggleModal psToggle model =
      case psToggle of
            ShowProfile -> showTab request.profile.get ShowProfile "profile.bundle.js" "#profile-edition-root"
            ShowSettings -> showTab request.settings.get ShowSettings "settings.bundle.js" "#settings-edition-root"
            ShowLeaderboard -> showTab request.leaderboard ShowLeaderboard "leaderboard.bundle.js" "#karma-leaderboard-root"
            ShowHelp -> showTab request.internalHelp ShowHelp "internalHelp.bundle.js" "#help-root"
            modal -> F.noMessages $ model {
                  toggleModal = modal
            }
      where showTab f toggle file root =
                  model { toggleModal = toggle } :> [
                        Just <<< SetModalContents (Just file) root <$> CCN.response (f {})
                  ]

setModalContents :: Maybe String -> String -> String -> IMModel -> NextMessage
setModalContents file root html model = CIF.nothingNext model $ loadModal root html file
      where loadModal root html file = liftEffect do
                  element <- CCD.unsafeQuerySelector root
                  CCD.setInnerHTML element html
                  --scripts don't load when inserted via innerHTML
                  case file of
                        Just name -> CCD.loadScript name
                        Nothing -> pure unit

showUserContextMenu :: Event -> IMModel -> MoreMessages
showUserContextMenu event model@{ userContextMenuVisible }
      | userContextMenuVisible =
            F.noMessages $ model { userContextMenuVisible = false }
      | otherwise =
            model :> [
                  liftEffect <<< map (Just <<< SetUserContentMenuVisible <<< (_ == "user-context-menu")) $ WDE.id <<< SU.fromJust $ do
                  target <- WEE.target event
                  WDE.fromEventTarget target
            ]


toogleUserContextMenu :: Boolean -> IMModel -> NoMessages
toogleUserContextMenu toggle model = F.noMessages $ model {  userContextMenuVisible = toggle }