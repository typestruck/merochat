module Client.Im.UserMenu where

import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Flame as CIF
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Options.MountPoint (karmaPrivilegesId)
import Shared.Resource (Bundle(..))
import Shared.Routes (routes)
import Shared.KarmaPrivileges.Types as SKT

toggleInitialScreen ∷ Boolean → ImModel → NoMessages
toggleInitialScreen toggle model = F.noMessages $ model
      { initialScreen = toggle
      , chatting = Nothing
      , toggleModal = HideUserMenuModal
      }

logout ∷ AfterLogout -> ImModel → MoreMessages
logout after model = CIF.nothingNext model out
      where
      out = do
            void $ request.logout { body: {} }
            liftEffect <<< CCL.setLocation $ case after of
                  LoginPage -> routes.login.get {}
                  Elsewhere -> routes.elsewhere {}
                  Banned -> routes.banned {}

toggleModal ∷ ShowUserMenuModal → ImModel → NextMessage
toggleModal mToggle model@{ modalsLoaded, user: { completedTutorial } } =
      case mToggle of
            ShowProfile → showTab request.profile.get ShowProfile (Just Profile) ProfileEditionRoot
            ShowSettings → showTab request.settings.get ShowSettings (Just Settings) SettingsEditionRoot
            ShowKarmaPrivileges → showTab request.leaderboard ShowKarmaPrivileges (Just KarmaPrivileges) KarmaPrivilegesRoot
            ShowHelp → showTab request.internalHelp ShowHelp (Just InternalHelp) HelpRoot
            ShowExperiments → showTab request.experiments ShowExperiments (Just Experiments) ExperimentsRoot
            ShowBacker → showTab request.internalBacker ShowBacker Nothing BackerRoot
            ShowFeedback → showTab request.feedback.get ShowFeedback (Just Feedback) FeedbackRoot
            modal → F.noMessages $ model
                  { toggleModal = modal
                  , erroredFields = []
                  , toggleContextMenu = HideContextMenu
                  }
      where
      showTab req toggle resource root =
            model
                  { toggleModal = toggle
                  , toggleContextMenu = HideContextMenu
                  , failedRequests = []
                  , modalsLoaded = toggle : modalsLoaded
                  } :>
                  if toggle /= ShowKarmaPrivileges && DA.elem toggle modalsLoaded then []
                  else
                        [ CCN.retryableResponse (ToggleModal toggle) (SetModalContents resource root) (req {})
                        -- during the tutorial the user may click on the user menu instead of "finish tutorial"
                        , if completedTutorial then pure Nothing else pure $ Just FinishTutorial
                        ]

setModalContents ∷ Maybe Bundle → ElementId → String → ImModel → NextMessage
setModalContents resource root html model = CIF.nothingNext model loadModal
      where
      loadModal = liftEffect do
            element ← CCD.unsafeGetElementById root
            CCD.setInnerHTML element html
            --scripts don't load when inserted via innerHTML
            case resource of
                  Just name → CCD.loadScript name
                  Nothing → pure unit

toggleUserContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleUserContextMenu toggle model = F.noMessages $ model { toggleContextMenu = toggle }