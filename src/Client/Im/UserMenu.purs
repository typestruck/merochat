module Client.Im.UserMenu where

import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Pwa (SwMessage(..))
import Client.Im.Pwa as CIP
import Client.Im.Suggestion (byAvailability)
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Class as EC
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Resource (Bundle(..))
import Shared.Routes (routes)
import Shared.Unsafe as SU

toggleInitialScreen ∷ Boolean → ImModel → MoreMessages
toggleInitialScreen toggle model =
      model
            { initialScreen = toggle
            , chatting = Nothing
            , toggleModal = HideUserMenuModal
            -- false toggle means we are showing suggestions
            , suggestions = if not toggle then byAvailability model.suggestions else model.suggestions
            } /\ [ updateDraft, updateServiceWorker ]
      where
      updateServiceWorker = do
            EC.liftEffect $ CIP.postMessage NotChatting
            pure Nothing
      updateDraft
            | toggle = EC.liftEffect do
                    input ← CCD.unsafeGetElementById ChatInput
                    draft ← CCD.value input
                    CCD.setValue input ""
                    if DM.isNothing model.chatting then
                          pure Nothing
                    else
                          pure <<< Just $ UpdateDraft (SU.fromJust model.chatting) draft
            | otherwise = pure Nothing

logout ∷ AfterLogout → ImModel → MoreMessages
logout after model = model /\ [ out ]
      where
      out = do
            void $ request.logout { body: {} }
            liftEffect <<< CCL.setLocation $ case after of
                  LoginPage → routes.login.get {}
                  Banned → routes.banned {}
            pure Nothing

toggleModal ∷ ShowUserMenuModal → ImModel → NextMessage
toggleModal mToggle model =
      case mToggle of
            ShowProfile → showTab request.profile.get ShowProfile (Just Profile) ProfileEditionRoot
            ShowSettings → showTab request.settings.get ShowSettings (Just Settings) SettingsEditionRoot
            ShowKarmaPrivileges → showTab request.leaderboard ShowKarmaPrivileges (Just KarmaPrivileges) KarmaPrivilegesRoot
            ShowHelp → showTab request.internalHelp ShowHelp (Just InternalHelp) HelpRoot
            ShowExperiments → showTab request.experiments ShowExperiments (Just Experiments) ExperimentsRoot
            ShowBacker → showTab request.internalBacker ShowBacker Nothing BackerRoot
            ShowFeedback → showTab request.feedback.get ShowFeedback (Just Feedback) FeedbackRoot
            ShowSuggestionCard id → F.noMessages model
                  { toggleModal = ShowSuggestionCard id
                  , showCollapsedMiniSuggestions = true
                  , suggesting = Just id
                  }
            modal → F.noMessages model
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
                  , modalsLoaded = toggle : model.modalsLoaded
                  } /\
                  if toggle /= ShowKarmaPrivileges && DA.elem toggle model.modalsLoaded then []
                  else
                        [ CCN.retryableResponse (ToggleModal toggle) (SetModalContents resource root) (req {})
                        -- during the tutorial the user may click on the user menu instead of "finish tutorial"
                        --, if model.user.completedTutorial then pure Nothing else pure $ Just FinishTutorial
                        ]

setModalContents ∷ Maybe Bundle → ElementId → String → ImModel → NextMessage
setModalContents resource root html model = model /\ [ loadModal ]
      where
      loadModal = liftEffect do
            element ← CCD.unsafeGetElementById root
            CCD.setInnerHTML element html
            --scripts don't load when inserted via innerHTML
            case resource of
                  Just name → CCD.loadScript name
                  Nothing → pure unit
            pure Nothing

