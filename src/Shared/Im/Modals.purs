module Shared.Im.View.Modal where

import Prelude

import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Symbol as TDS
import Data.Time.Duration (Days(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Element (ElementId(..))
import Shared.Im.Svg as SIA
import Shared.Im.Types (AfterLogout(..), ConfirmationModal(..), ImMessage(..), ImModel, Modal(..), ReportReason(..), RetryableRequest(..), ScreenModal(..), SpecialModal(..), Step(..))
import Shared.Im.View.Profile as CISP
import Shared.Im.View.Retry as SIVR
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Setter as SS
import Shared.Svg as SSI
import Shared.Unsafe as SU
import Shared.User as SUR
import Type.Proxy (Proxy(..))

lazyLoad ∷ Bundle → Html ImMessage
lazyLoad resource = HE.link [ HA.rel "preload", HA.type' "text/css", HA.createAttribute "as" "style", HA.href $ SP.bundlePath resource Css, HA.createAttribute "onload" "this.onload=null;this.rel='stylesheet'" ]

modals ∷ ImModel → Html ImMessage
modals model =
      HE.div (HA.class' { "modal-placeholder-overlay": true, hidden: not shouldShow })
            ( otherModals <>
                    [ lazyLoad Help
                    , lazyLoad Profile
                    , lazyLoad Settings
                    , lazyLoad Profile
                    , lazyLoad Backer
                    , lazyLoad Experiments
                    , lazyLoad KarmaPrivileges
                    , lazyLoad Feedback
                    , modalMenu model
                    ]
            )
      where
      shouldShow = case model.modal of
        Chat _ -> false
        HideModal -> false
        _ -> true

      otherModals = case model.modal of
            Confirmation cf -> case cf of
                ConfirmReport id → [ confirmReport id model.erroredFields ]
                ConfirmLogout → [ confirmLogout ]
                ConfirmDeleteChat id → [ confirmDeleteChat id ]
                ConfirmBlockUser id → [ confirmBlockUser id ]
                ConfirmTerminationTemporaryUser → [ confirmTermination ]
            Special sp -> case sp of
                ShowSuggestionCard _ → [ CISP.individualSuggestion (SU.fromJust (model.suggesting >>= (\sid → DA.find ((sid == _) <<< _.id) model.suggestions))) model ]
                Tutorial step → [  ]
            _ -> []

confirmReport ∷ Int → Array String → Html ImMessage
confirmReport id erroredFields =
      HE.div (HA.class' "confirmation report")
            [ HE.span (HA.class' "report-title") "Report user"
            , HE.div (HA.class' "report-reasons") $ DA.mapWithIndex toRadio [ DatingContent, Harassment, HateSpeech, Spam, Minor, OtherReason ]
            , HE.span [ HA.class' { "error-message": true, "invisible": not (DA.elem (TDS.reflectSymbol (Proxy ∷ Proxy "reportReason")) erroredFields) } ] "Please choose a reason"
            , HE.div (HA.class' "report-comment")
                    [ HE.label_ "Comment"
                    , HE.input [ HA.type' "text", HA.maxlength 300, HA.class' "modal-input", HA.onInput setReportComment ]
                    ]
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ ReportUser id ] "Report"
                    ]
            ]
      where
      setReportComment value = SetField (_ { reportComment = Just value })

      toRadio i reason =
            let
                  idName = show i
            in
                  HE.div (HA.class' "reason")
                        [ HE.input [ HA.type' "radio", HA.id $ "report-" <> idName, HA.name "report-reason", HA.onInput (const (SetField (_ { reportReason = Just reason, erroredFields = [] }))) ]
                        , HE.label (HA.for $ "report-" <> idName) $ show reason
                        ]

confirmLogout ∷ Html ImMessage
confirmLogout =
      HE.div (HA.class' "confirmation")
            [ HE.span (HA.class' "bold") "Do you really want to log out?"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick $ Logout LoginPage ] "Logout"
                    ]
            ]

confirmDeleteChat ∷ Int → Html ImMessage
confirmDeleteChat id =
      HE.div (HA.class' "confirmation")
            [ HE.span (HA.class' "bold") "Do you really want to delete this chat?"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ DeleteChat id ] "Delete"
                    ]
            ]

confirmBlockUser ∷ Int → Html ImMessage
confirmBlockUser id =
      HE.div (HA.class' "confirmation")
            [ HE.span (HA.class' "bold") "Do you really want to block this user?"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ BlockUser id ] "Block"
                    ]
            ]

-- tutorial ∷ ImModel → Step → Html ImMessage
-- tutorial { chatting } = case _ of
--       Welcome → HE.div (HA.class' "confirmation tutorial")
--             [ HE.span (HA.class' "bold") "Welcome!"
--             , HE.span_ "Let's take you through a brief tutorial"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "cancel", HA.onClick FinishTutorial ] "Skip tutorial"
--                     , HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial ChatSuggestions ] "Start!"
--                     ]
--             ]
--       ChatSuggestions → HE.div (HA.class' "confirmation tutorial chat-step")
--             [ HE.span (HA.class' "bold") "Chat suggestions"
--             , HE.span_ "Use the arrows to move back and forth suggestions"
--             , HE.span_ "When you see someone you'd like to chat with,"
--             , HE.span (HA.class' "italic") "send them a message to enable the next step"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "cancel", HA.onClick FinishTutorial ] "Skip tutorial"
--                     , HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial Chatting, HA.disabled $ DM.isNothing chatting, HA.title $ if DM.isNothing chatting then "Send a message to enable the next step" else "" ] "Done!"
--                     ]
--             ]
--       Chatting → HE.div (HA.class' "confirmation tutorial chatting-step")
--             [ HE.span (HA.class' "bold") "Chatting"
--             , HE.span_ "Nice, you started your first chat!"
--             , HE.span_ "You can click the menu on top to see the"
--             , HE.span_ "full profile of the person you are chatting with"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "cancel", HA.onClick FinishTutorial ] "Skip tutorial"
--                     , HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial BackSuggestions ] "Got it!"
--                     ]
--             ]
--       BackSuggestions → HE.div (HA.class' "confirmation tutorial back-suggestions-step")
--             [ HE.span (HA.class' "bold") "Moving between chats and suggestions"
--             , HE.span_ "Whenever you are chatting, you can click"
--             , HE.span_ "on the green box see your suggestions again"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "cancel", HA.onClick FinishTutorial ] "Skip tutorial"
--                     , HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial ChatList ] "Done!"
--                     ]
--             ]
--       ChatList → HE.div (HA.class' "confirmation tutorial chat-list-step")
--             [ HE.span (HA.class' "bold") "Chat list"
--             , HE.span_ "Your recent chats appear on the left"
--             , HE.span_ "At any time, you can click on a chat to resume it"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "cancel", HA.onClick FinishTutorial ] "Skip tutorial"
--                     , HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial OptionsMenu ] "Got it!"
--                     ]
--             ]
--       OptionsMenu → HE.div (HA.class' "confirmation tutorial options-menu-step")
--             [ HE.span (HA.class' "bold") "Options menu"
--             , HE.span_ "Tweak your preferences with the menu on the top left"
--             , HE.span_ "You can edit your profile, modify your settings, get help, send feedback and more"
--             , HE.div (HA.class' "buttons")
--                     [ HE.button [ HA.class' "green-button step-button", HA.onClick FinishTutorial ] "Finish tutorial"
--                     ]
--             ]

modalMenu ∷ ImModel → Html ImMessage
modalMenu model =
      HE.div (HA.class' { "modal-placeholder": true, hidden: not screenModal })
            [ HE.div (HA.class' "modal-menu-mobile")
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ]
                    , HE.strong_ $ case model.modal of
                        Screen m -> show m
                        _ -> ""
                    ]
            , HE.div (HA.class' { "modal-menu": true, hidden: model.smallScreen && model.modal /= Screen ShowMenu })
                    [ HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal HideModal, HA.class' { back: true, hidden: model.smallScreen || model.user.temporary && SUR.temporaryUserExpiration model.user.joined <= Days 0.0 } ]
                            [ HE.svg [ HA.class' "svg-16", HA.viewBox "0 0 30 30" ]
                                    [ HE.path' [ HA.d "M30 13.125H7.18125L17.6625 2.64375L15 0L0 15L15 30L17.6437 27.3563L7.18125 16.875H30V13.125Z" ]
                                    ]
                            , HE.text " Back to chats"
                            ]
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile, HA.class' { entry: true, selected: model.modal == Screen ShowProfile } ] $ show ShowProfile
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowSettings, HA.class' { entry: true, selected: model.modal == Screen ShowSettings } ] $ show ShowSettings
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges, HA.class' { entry: true, selected: model.modal == Screen ShowKarmaPrivileges } ] $ show ShowKarmaPrivileges
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowExperiments, HA.class' { entry: true, selected: model.modal == Screen ShowExperiments } ] $ show ShowExperiments
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowBacker, HA.class' { entry: true, selected: model.modal == Screen ShowBacker } ] $ show ShowBacker
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowHelp, HA.class' { entry: true, selected: model.modal == Screen ShowHelp } ] $ show ShowHelp
                    , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowFeedback, HA.class' { entry: true, selected: model.modal == Screen ShowFeedback } ] $ show ShowFeedback
                    , HE.div (HA.class' "entry theme-modal")
                            [ SSI.sun
                            , SSI.moon
                            ]
                    , if model.user.temporary then
                            HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ Confirmation ConfirmTerminationTemporaryUser ] "Delete my data"
                      else
                            HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ Confirmation ConfirmLogout ] "Logout"
                    ]
            , if model.user.temporary then
                    temporaryUserSignUp model
              else HE.fragment
                    [ HE.div [ HA.id $ show ProfileEditionRoot, HA.class' { hidden: model.modal /= Screen ShowProfile } ] $ retry ShowProfile
                    , HE.div [ HA.id $ show SettingsEditionRoot, HA.class' { hidden: model.modal /= Screen ShowSettings } ] $ retry ShowSettings
                    , HE.div [ HA.id $ show KarmaPrivilegesRoot, HA.class' { hidden: model.modal /= Screen ShowKarmaPrivileges } ] $ retry ShowKarmaPrivileges
                    , HE.div [ HA.id $ show ExperimentsRoot, HA.class' { hidden: model.modal /= Screen ShowExperiments } ] $ retry ShowExperiments
                    , HE.div [ HA.id $ show BackerRoot, HA.class' { hidden: model.modal /= Screen ShowBacker } ] $ retry ShowBacker
                    , HE.div [ HA.id $ show HelpRoot, HA.class' { hidden: model.modal /= Screen ShowHelp } ] $ retry ShowHelp
                    , HE.div [ HA.id $ show FeedbackRoot, HA.class' { hidden: model.modal /= Screen ShowFeedback } ] $ retry ShowFeedback
                    ]
            ]
      where
      screenModal = case model.modal of
                Screen _ -> true
                _ -> false
      retry tm = HE.div (HA.class' "retry-modal")
            [ SIVR.retry "Failed to load contents" (ToggleModal $ Screen tm) model.failedRequests
            , HE.div' (HA.class' "loading")
            ]

--only for temporary users, since logging out = deleting account
confirmTermination ∷ Html ImMessage
confirmTermination = HE.div (HA.class' "modal-placeholder-overlay")
      [ HE.div [ HA.id $ show ConfirmAccountTerminationForm, HA.class' "confirmation" ]
              [ HE.span (HA.class' "bold") "All your chats will be permanently lost, and you will be logged out"
              , HE.div (HA.class' "buttons")
                      [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] "Cancel"
                      , HE.button [ HA.class' "green-button danger", HA.onClick TerminateTemporaryUser ] "Yes, delete my data"
                      ]
              , HE.span' (HA.class' "request-error-message")
              , HE.span (HA.class' "success-message right-align")
                      [ HE.text "Your data has been deleted"
                      , HE.br
                      , HE.text "You will be logged out..."
                      ]
              ]
      ]

temporaryUserSignUp ∷ ImModel → Html ImMessage
temporaryUserSignUp { temporaryEmail, temporaryPassword, erroredFields, modal, user: { temporary, joined } } =
      HE.div [ HA.id $ show TemporaryUserSignUpForm, HA.class' { hidden: not temporary } ]
            [ if expired then
                    HE.div (HA.class' "warning-temporary") "Your access has expired"
              else HE.fragment
                    [ HE.div (HA.class' "warning-temporary") $ "You have " <> remainingTime <> " to create an account"
                    , HE.div (HA.class' "warning-temporary wall-text") "After that, all your data will be deleted and you won't be able to access the site unless you sign up again"
                    ]
            , HE.div (HA.class' "duller last") "Create your account now, it is free!"
            , HE.div_
                    [ HE.label_ "Email"
                    , HE.input [ HA.class' "modal-input", HA.type' "text", HA.id "email", HA.value $ DM.fromMaybe "" temporaryEmail, HA.onInput (SS.setJust (Proxy ∷ _ "temporaryEmail")), HA.maxlength emailMaxCharacters ]
                    , HE.span [ HA.class' { "error-message": true, invisible: not $ DA.elem (TDS.reflectSymbol (Proxy ∷ _ "temporaryEmail")) erroredFields } ] "Please enter a valid email"
                    ]
            , HE.div_
                    [ HE.label_ "Password"
                    , HE.input [ HA.class' "modal-input", HA.type' "password", HA.maxlength passwordMaxCharacters, HA.autocomplete "new-password", HA.value $ DM.fromMaybe "" temporaryPassword, HA.onInput (SS.setJust (Proxy ∷ _ "temporaryPassword")) ]
                    , HE.span [ HA.class' { "error-message": true, invisible: not $ DA.elem (TDS.reflectSymbol (Proxy ∷ _ "temporaryPassword")) erroredFields } ] $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                    ]
            , HE.div_
                    [ HE.input [ HA.type' "button", HA.class' "green-button", HA.value "Create account", HA.onClick CreateUserFromTemporary ]
                    , HE.span' [ HA.class' "request-error-message error-message" ]
                    ]
            ]
      where
      remaining = DI.floor <<< SC.coerce $ SUR.temporaryUserExpiration joined
      expired = remaining < 0
      remainingTime = case remaining of
            0 → " only a few hours left"
            1 → " until tomorrow"
            n → show n <> " more days"