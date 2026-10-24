module Client.Im.ModalsMenu where

import Prelude

import Client.AppId (imAppId, loginAppId, profileAppId, bannedAppId)
import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.EventTypes (modalVisible)
import Client.Im.Chat as CIC
import Client.Im.Flame (MoreMessages, NextMessage)
import Client.Im.Suggestion as CIS
import Client.Location as CL
import Client.Lynx.Session as CLS
import Client.Network (routes)
import Client.Network as CN
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame as F
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Payload.Client (ClientResponse)
import Prim.Coerce (class Coercible)
import Safe.Coerce as SC
import Shared.Element (ElementId(..))
import Shared.Html (Html(..))
import Shared.Im.Types (AfterLogout(..), ImMessage(..), ImModel, PostMode(..), RetryableRequest(..), ShowContextMenu(..))
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Resource (Bundle(..))
import Shared.Routes (routesSpec)
import Shared.Unsafe as SU

toggleInitialScreen ∷ Boolean → ImModel → MoreMessages
toggleInitialScreen toggle model =
      model
            { initialScreen = toggle
            , chatting = Nothing
            , modal = HideModal
            , showChangelogs = model.showChangelogs && toggle
            -- false toggle means we are showing suggestions
            , suggestions = if not toggle then CIS.byAvailability model.suggestions else model.suggestions
            } /\ [ updateDraft ]
      where
      updateDraft
            | toggle = do
                    input ← EC.liftEffect $ CUD.unsafeGetElementById ChatInput
                    draft ← CUE.value input
                    CUE.setValue input ""
                    if DM.isNothing model.chatting then
                          pure Nothing
                    else
                          pure <<< Just $ UpdateDraft (SU.fromJust model.chatting) draft
            | otherwise = pure Nothing

logout ∷ AfterLogout → ImModel → MoreMessages
logout after model = model /\ [ out ]
      where
      out = do
            void $ routes.logout { body: {} }
            EC.liftEffect do
                  native ← CLS.isNativeSession
                  if native then case after of
                        LoginPage → F.navigate loginAppId
                        Banned → F.navigate bannedAppId
                  else CL.setLocation $ case after of
                        LoginPage → routesSpec.login.get {}
                        Banned → routesSpec.banned {}
            pure Nothing

modal ∷ Modal → ImModel → NextMessage
modal toggled model =
      case toggled of
            Screen (ShowProfile _) → model /\ [ EC.liftEffect (F.navigate profileAppId) $> Nothing ]
            Screen ShowSettings → showModal routes.settings.get {} ShowSettings Settings SettingsEditionRoot
            Screen ShowKarmaPrivileges → showModal routes.leaderboard {} ShowKarmaPrivileges KarmaPrivileges KarmaPrivilegesRoot
            Screen ShowHelp → showModal routes.internalHelp {} ShowHelp InternalHelp HelpRoot
            Screen (ShowExperiments e) → showModal routes.experiments.get {} (ShowExperiments e) Experiments ExperimentsRoot
            Screen ShowBacker → showModal routes.internalBacker {} ShowBacker InternalBacker BackerRoot
            Screen ShowFeedback → showModal routes.feedback.get {} ShowFeedback Feedback FeedbackRoot
            Special ShowPostForm → model { modal = toggled, showSuggestionsPostForm = false } /\ []
            Special (ShowSuggestionCard id) → model
                  { modal = toggled
                  , showCollapsedMiniSuggestions = true
                  , suggesting = Just id
                  } /\ []
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
            EC.liftEffect do
                  FSUC.broadcast modalVisible $ Screen toggle
                  when (toggle == ShowMenu) $ F.navigate imAppId
            pure Nothing

      showModal ∷ ∀ query response. Coercible response String ⇒ (query → Aff (ClientResponse response)) → query → ScreenModal → Bundle → ElementId → NextMessage
      showModal req query toggle resource root
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
                                , CN.retryableRequest (ToggleModal $ Screen toggle) (SetModalContents resource root <<< SC.coerce) (req query)
                                ]

setModalContents ∷ Bundle → ElementId → String → ImModel → NextMessage
setModalContents resource root html model = model /\ [ loadModal ]
      where
      loadModal = EC.liftEffect do
            element ← CUD.unsafeGetElementById root
            CUE.setInnerHTML element html
            --scripts don't load when inserted via innerHTML and dynamic module import is a whole can of worms
            CUD.loadScript resource
            pure Nothing
