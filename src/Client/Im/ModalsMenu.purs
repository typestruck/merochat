module Client.Im.ModalsMenu where

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
import Flame.Subscription.Unsafe.CustomEvent as FS
import Shared.Element (ElementId(..))
import Shared.Im.EventTypes (modalVisible)
import Shared.Modal.Types (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)
import Shared.Unsafe as SU

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
            Screen ShowProfile → showModal request.profile.get ShowProfile Profile ProfileEditionRoot
            Screen ShowSettings → showModal request.settings.get ShowSettings Settings SettingsEditionRoot
            Screen ShowKarmaPrivileges → showModal request.leaderboard ShowKarmaPrivileges KarmaPrivileges KarmaPrivilegesRoot
            Screen ShowHelp → showModal request.internalHelp ShowHelp InternalHelp HelpRoot
            Screen ShowExperiments → showModal request.experiments ShowExperiments Experiments ExperimentsRoot
            Screen ShowBacker → showModal request.internalBacker ShowBacker InternalBacker BackerRoot
            Screen ShowFeedback → showModal request.feedback.get ShowFeedback Feedback FeedbackRoot
            Special (ShowSuggestionCard id) → F.noMessages model
                  { modal = Special $ ShowSuggestionCard id
                  , showCollapsedMiniSuggestions = true
                  , suggesting = Just id
                  }
            Chat c → CIC.modal c model
            t →
                  model
                        { modal = t
                        , erroredFields = []
                        , toggleContextMenu = HideContextMenu
                        } /\ [ visible ShowMenu ]
      where
      visible toggle = do
            EC.liftEffect $ FS.broadcast modalVisible toggle
            pure Nothing

      showModal req toggle resource root
            | model.user.temporary =
                    model
                          { modal = Screen toggle
                          , toggleContextMenu = HideContextMenu
                          , failedRequests = []
                          } /\ []
            | otherwise =
                    model
                          { modal = Screen toggle
                          , toggleContextMenu = HideContextMenu
                          , failedRequests = []
                          , modalsLoaded = toggle : model.modalsLoaded
                          } /\
                          if DA.elem toggle model.modalsLoaded then
                                [ visible toggle ]
                          else
                                [ visible toggle
                                , CCN.retryableResponse (ToggleModal $ Screen toggle) (SetModalContents resource root) (req {})
                                -- during the tutorial the user may click on the user menu instead of "finish tutorial"
                                --, if model.user.completedTutorial then pure Nothing else pure $ Just FinishTutorial
                                ]

setModalContents ∷ Bundle → ElementId → String → ImModel → NextMessage
setModalContents resource root html model = model /\ [ loadModal ]
      where
      loadModal = liftEffect do
            element ← CCD.unsafeGetElementById root
            CCD.setInnerHTML element html
            --scripts don't load when inserted via innerHTML and dynamic module import is a whole can of worms
            CCD.loadScript resource
            pure Nothing

