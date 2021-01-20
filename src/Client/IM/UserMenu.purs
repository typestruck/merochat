module Client.IM.UserMenu where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Shared.Types
import Shared.Path as SP
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NextMessage, NoMessages)
import Client.IM.Flame as CIF
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Routes (routes)

toggleInitialScreen :: IMModel -> NoMessages
toggleInitialScreen model@{ initialScreen } = F.noMessages $ model {
      initialScreen = not initialScreen,
      chatting = Nothing
 }

logout :: IMModel -> MoreMessages
logout model = CIF.nothingNext model out
      where out = do
                  void $ request.logout { body: {} }
                  liftEffect $ CCL.setLocation $ routes.login.get {}

toggleModal :: ShowUserMenuModal -> IMModel -> NextMessage
toggleModal mToggle model =
      case mToggle of
            ShowProfile -> showTab request.profile.get ShowProfile "profile.6f4f3e5a964a4fe39094" "#profile-edition-root"
            ShowSettings -> showTab request.settings.get ShowSettings "settings.e737b2c018c48843b189" "#settings-edition-root"
            ShowLeaderboard -> showTab request.leaderboard ShowLeaderboard "leaderboard.4d4f6182f4af2475e4e8" "#karma-leaderboard-root"
            ShowHelp -> showTab request.internalHelp ShowHelp "internalHelp.b7950417810ec6df079a" "#help-root"
            modal -> F.noMessages $ model {
                  toggleModal = modal,
                  toggleContextMenu = HideContextMenu
            }
      where showTab f toggle file root =
                  model {
                        toggleModal = toggle,
                        toggleContextMenu = HideContextMenu,
                        failedRequests = []
                  } :> [
                        CCN.retryableResponse (ToggleModal toggle) (SetModalContents (Just file) root) (f {})
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

toogleUserContextMenu :: ShowContextMenu -> IMModel -> NoMessages
toogleUserContextMenu toggle model = F.noMessages $ model { toggleContextMenu = toggle }