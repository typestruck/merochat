module Client.Im.ModalsMenu where

import Prelude

import Client.AppId (ClientAppId, imAppId, loginAppId, profileAppId, bannedAppId, settingsAppId, karmaPrivilegesAppId, experimentsAppId, feedbackAppId, internalHelpAppId, internalBackerAppId)
import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.EventTypes (modalVisible)
import Client.Im.Chat as CIC
import Client.Im.Flame (MoreMessages, NextMessage)
import Client.Im.Suggestion as CIS
import Client.Location as CL
import Client.Lynx.Session as CLS
import Client.Network (routes)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Flame (AppId)
import Flame.Subscription as FS
import Shared.Settings.Types as SST
import Shared.KarmaPrivileges.Types as SKT
import Shared.Experiments.Types as SET
import Shared.Feedback.Types as SFT
import Shared.InternalHelp.Types as SIHT
import Shared.Backer.Types as SBT
import Flame as F
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Shared.Element (ElementId(..))
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
            Screen ShowSettings → showScreen settingsAppId SST.ToggleVisibility ShowSettings
            Screen ShowKarmaPrivileges → showScreen karmaPrivilegesAppId SKT.ToggleVisibility ShowKarmaPrivileges
            Screen ShowHelp → showScreen internalHelpAppId SIHT.ToggleVisibility ShowHelp
            Screen (ShowExperiments e) → showScreen experimentsAppId SET.ToggleVisibility $ ShowExperiments e
            Screen ShowBacker → showScreen internalBackerAppId SBT.ToggleVisibility ShowBacker
            Screen ShowFeedback → showScreen feedbackAppId SFT.ToggleVisibility ShowFeedback
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

      showScreen ∷ ∀ message. AppId ClientAppId message → (Modal → message) → ScreenModal → NextMessage
      showScreen appId message toggle =
            model
                  { modal = Screen toggle
                  , toggleContextMenu = HideContextMenu
                  , failedRequests = []
                  } /\ if model.user.temporary then [] else [ open ]
            where
            open = EC.liftEffect do
                  F.navigate appId
                  FS.send appId <<< message $ Screen toggle
                  pure Nothing

setModalContents ∷ Bundle → ElementId → String → ImModel → NextMessage
setModalContents resource root html model = model /\ [ loadModal ]
      where
      loadModal = EC.liftEffect do
            element ← CUD.unsafeGetElementById root
            CUE.setInnerHTML element html
            --scripts don't load when inserted via innerHTML and dynamic module import is a whole can of worms
            CUD.loadScript resource
            pure Nothing
