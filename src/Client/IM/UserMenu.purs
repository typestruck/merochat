module Client.IM.UserMenu where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NextMessage, NoMessages)
import Client.IM.Flame as CIF
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Routes (routes)

toggleInitialScreen :: Boolean -> IMModel -> NoMessages
toggleInitialScreen toggle model@{ initialScreen } = F.noMessages $ model {
      initialScreen = toggle,
      chatting = Nothing,
      toggleModal = HideUserMenuModal
 }

logout :: IMModel -> MoreMessages
logout model = CIF.nothingNext model out
      where out = do
                  void $ request.logout { body: {} }
                  liftEffect $ CCL.setLocation $ routes.login.get {}

toggleModal :: ShowUserMenuModal -> IMModel -> NextMessage
toggleModal mToggle model =
      case mToggle of
            ShowProfile -> showTab request.profile.get ShowProfile (Just "profile.6f4f3e5a964a4fe39094") ProfileEditionRoot
            ShowSettings -> showTab request.settings.get ShowSettings (Just "settings.e737b2c018c48843b189") SettingsEditionRoot
            ShowLeaderboard -> showTab request.leaderboard ShowLeaderboard (Just "leaderboard.4d4f6182f4af2475e4e8") KarmaLeaderboard
            ShowHelp -> showTab request.internalHelp ShowHelp (Just "internalHelp.b7950417810ec6df079a") HelpRoot
            ShowBacker -> showTab request.internalBacker ShowBacker Nothing BackerRoot
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
                        CCN.retryableResponse (ToggleModal toggle) (SetModalContents file root) (f {})
                  ]

setModalContents :: Maybe String -> ElementID -> String -> IMModel -> NextMessage
setModalContents file root html model = CIF.nothingNext model $ loadModal root html file
      where loadModal root html file = liftEffect do
                  element <- CCD.unsafeGetElementByID root
                  CCD.setInnerHTML element html
                  --scripts don't load when inserted via innerHTML
                  case file of
                        Just name -> CCD.loadScript name
                        Nothing -> pure unit

toogleUserContextMenu :: ShowContextMenu -> IMModel -> NoMessages
toogleUserContextMenu toggle model = F.noMessages $ model { toggleContextMenu = toggle }