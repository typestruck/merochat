module Client.Im.ModalsMenu where

import Prelude

import Client.Dom as CCD
import Client.Location as CCL
import Client.Network (request)
import Client.Network as CCN
import Client.Im.Chat as CIC
import Client.Im.Flame (MoreMessages, NextMessage)
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
import Flame.Subscription.Unsafe.CustomEvent as FS
import Shared.Element (ElementId(..))
import Client.EventTypes (modalVisible)
import Shared.Im.Types (AfterLogout(..), ImMessage(..), ImModel, PostMode(..), RetryableRequest(..), ShowContextMenu(..))
import Shared.Modal.Types (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Resource (Bundle(..))
import Shared.Routes (routes)
import Shared.Unsafe as SU

toggleInitialScreen ∷ Boolean → ImModel → MoreMessages
toggleInitialScreen toggle model =
      model
            { initialScreen = toggle
            , chatting = Nothing
            , modal = HideModal
            , showChangelogs = model.showChangelogs && toggle
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
            Screen ShowExperiments → showModal request.experiments.get ShowExperiments Experiments ExperimentsRoot
            Screen ShowBacker → showModal request.internalBacker ShowBacker InternalBacker BackerRoot
            Screen ShowFeedback → showModal request.feedback.get ShowFeedback Feedback FeedbackRoot
            Special ShowPostForm → model { modal = toggled, showSuggestionsPostForm = false } /\ []
            Special (ShowSuggestionCard id) → F.noMessages model
                  { modal = toggled
                  , showCollapsedMiniSuggestions = true
                  , suggesting = Just id
                  }
            Chat c → CIC.modal c model
            t →
                  model
                        { modal = t
                        , erroredFields = []
                        , selectedImage = Nothing
                        , posts = model.posts { image = Nothing, mode = TextOnly }
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

