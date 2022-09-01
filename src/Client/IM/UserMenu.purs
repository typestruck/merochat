module Client.IM.UserMenu where

import Prelude
import Shared.ContentType

import Client.Common.DOM (setChatExperiment)
import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NextMessage, NoMessages)
import Client.IM.Flame as CIF
import Data.Array ((:))
import Data.Array as DA
import Data.HashSet as DHS
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Class (liftEffect)
import Environment (experimentsJSHash, internalHelpJSHash, leaderboardCSSHash, leaderboardJSHash, profileJSHash, settingsJSHash)
import Flame ((:>))
import Shared.Im.Types
import Flame as F
import Shared.Json as SJ
import Shared.Routes (routes)

toggleInitialScreen ∷ Boolean → ImModel → NoMessages
toggleInitialScreen toggle model@{ initialScreen } = F.noMessages $ model
      { initialScreen = toggle
      , chatting = Nothing
      , toggleModal = HideUserMenuModal
      }

logout ∷ ImModel → MoreMessages
logout model = CIF.nothingNext model out
      where
      out = do
            void $ request.logout { body: {} }
            liftEffect $ CCL.setLocation $ routes.login.get {}

toggleModal ∷ ShowUserMenuModal → ImModel → NextMessage
toggleModal mToggle model@{ modalsLoaded , user : { completedTutorial }} =
      case mToggle of
            ShowProfile → showTab request.profile.get ShowProfile (Just $ "profile." <> profileJSHash) ProfileEditionRoot
            ShowSettings → showTab request.settings.get ShowSettings (Just $ "settings." <> settingsJSHash) SettingsEditionRoot
            ShowLeaderboard → showTab request.leaderboard ShowLeaderboard (Just $ "leaderboard." <> leaderboardJSHash) KarmaLeaderboard
            ShowHelp → showTab request.internalHelp ShowHelp (Just $ "internalHelp." <> internalHelpJSHash) HelpRoot
            ShowExperiments → showTab request.experiments ShowExperiments (Just $ "experiments." <> experimentsJSHash) ExperimentsRoot
            ShowBacker → showTab request.internalBacker ShowBacker Nothing BackerRoot
            modal → F.noMessages $ model
                  { toggleModal = modal
                  , erroredFields = []
                  , toggleContextMenu = HideContextMenu
                  }
      where
      showTab f toggle file root =
            model
                  { toggleModal = toggle
                  , toggleContextMenu = HideContextMenu
                  , failedRequests = []
                  , modalsLoaded = toggle : modalsLoaded
                  } :>
                  if DA.elem toggle modalsLoaded then []
                  else
                        [ CCN.retryableResponse (ToggleModal toggle) (SetModalContents file root) (f {})
                          -- during the tutorial the user may click on the user menu instead of "finish tutorial"
                        , if completedTutorial then pure Nothing else pure $ Just FinishTutorial
                        ]

setModalContents ∷ Maybe String → ElementId → String → ImModel → NextMessage
setModalContents file root html model = CIF.nothingNext model loadModal
      where
      loadModal = liftEffect do
            element ← CCD.unsafeGetElementById root
            CCD.setInnerHTML element html
            --scripts don't load when inserted via innerHTML
            case file of
                  Just name → CCD.loadScript name
                  Nothing → pure unit

toggleUserContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleUserContextMenu toggle model = F.noMessages $ model { toggleContextMenu = toggle }