module Client.Im.UserMenu where

import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Chat as CIC
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
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)
import Shared.Unsafe as SU

foreign import dynamicImport_ :: EffectFn1 String Unit

toggleInitialScreen ∷ Boolean → ImModel → MoreMessages
toggleInitialScreen toggle model =
      model
            { initialScreen = toggle
            , chatting = Nothing
            , modal = HideModal
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

modal ∷ Modal → ImModel → NextMessage
modal toggled model =
      case toggled of
            Screen ShowProfile → showTab request.profile.get ShowProfile (Just Profile) ProfileEditionRoot
            Screen ShowSettings → showTab request.settings.get ShowSettings (Just Settings) SettingsEditionRoot
            Screen ShowKarmaPrivileges → showTab request.leaderboard ShowKarmaPrivileges (Just KarmaPrivileges) KarmaPrivilegesRoot
            Screen ShowHelp → showTab request.internalHelp ShowHelp (Just InternalHelp) HelpRoot
            Screen ShowExperiments → showTab request.experiments ShowExperiments (Just Experiments) ExperimentsRoot
            Screen ShowBacker → showTab request.internalBacker ShowBacker Nothing BackerRoot
            Screen ShowFeedback → showTab request.feedback.get ShowFeedback (Just Feedback) FeedbackRoot
            Special (ShowSuggestionCard id) → F.noMessages model
                  { modal = Special $ ShowSuggestionCard id
                  , showCollapsedMiniSuggestions = true
                  , suggesting = Just id
                  }
            Chat c -> CIC.modal c model
            t → F.noMessages model
                  { modal = t
                  , erroredFields = []
                  , toggleContextMenu = HideContextMenu
                  }
      where
      showTab req toggle resource root =
            model
                  { modal = Screen toggle
                  , toggleContextMenu = HideContextMenu
                  , failedRequests = []
                  } /\
                        [ CCN.retryableResponse (ToggleModal $ Screen toggle) (SetModalContents resource root) (req {})
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
                  Just name → EU.runEffectFn1 dynamicImport_ $ SP.bundlePath name Js
                  Nothing → pure unit
            pure Nothing

