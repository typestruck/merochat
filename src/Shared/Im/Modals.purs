module Shared.Im.View.Modal where

import Prelude
import Shared.Im.Types

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
import Shared.Im.Svg as SIS
import Shared.Im.View.Profile as CISP
import Shared.Im.View.Retry as SIVR
import Shared.Modal.Types (ConfirmationModal(..), Modal(..), ScreenModal(..), SpecialModal(..), Step(..))
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
                    , lazyLoad Backer
                    , lazyLoad Experiments
                    , lazyLoad KarmaPrivileges
                    , lazyLoad Feedback
                    , modalMenu model
                    ]
            )
      where
      shouldShow = case model.modal of
            Chat _ → false
            HideModal → false
            _ → true

      otherModals = case model.modal of
            Confirmation cf → case cf of
                  ConfirmReport id → [ confirmReport id model.erroredFields ]
                  ConfirmLogout → [ confirmLogout ]
                  ConfirmDeleteChat id → [ confirmDeleteChat id ]
                  ConfirmBlockUser id → [ confirmBlockUser id ]
                  ConfirmTerminationTemporaryUser → [ confirmTermination ]
            Special sp → case sp of
                  ShowSuggestionCard _ → [ CISP.individualSuggestion (SU.fromJust (model.suggesting >>= (\sid → DA.find ((sid == _) <<< _.id) model.suggestions))) model ]

            _ → []

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

modalMenu ∷ ImModel → Html ImMessage
modalMenu model =
      HE.div (HA.class' { "modal-placeholder": true, hidden: not screenModal })
            ( [ HE.div (HA.class' "modal-menu-mobile")
                      [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ]
                      , HE.strong_ $ case model.modal of
                              Screen m → show m
                              _ → ""
                      ]
              , HE.div (HA.class' { "modal-menu": true, hidden: model.smallScreen && model.modal /= Screen ShowMenu })
                      [ HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile, HA.class' { entry: true, selected: model.modal == Screen ShowProfile } ] $ show ShowProfile
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowSettings, HA.class' { entry: true, selected: model.modal == Screen ShowSettings } ] $ show ShowSettings
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges, HA.class' { entry: true, selected: model.modal == Screen ShowKarmaPrivileges } ] $ show ShowKarmaPrivileges
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowExperiments, HA.class' { entry: true, selected: model.modal == Screen ShowExperiments } ] $ show ShowExperiments
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowBacker, HA.class' { entry: true, selected: model.modal == Screen ShowBacker } ] $ show ShowBacker
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowHelp, HA.class' { entry: true, selected: model.modal == Screen ShowHelp } ] $ show ShowHelp
                      , HE.div [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowFeedback, HA.class' { entry: true, selected: model.modal == Screen ShowFeedback } ] $ show ShowFeedback
                      , HE.div (HA.class' "entry theme-modal")
                              [ SSI.sun [ HA.onClick $ SetTheme Light ]
                              , SSI.moon [ HA.onClick $ SetTheme Dark ]
                              ]
                      , if model.user.temporary then
                              HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ Confirmation ConfirmTerminationTemporaryUser ] "Delete my data"
                        else
                              HE.div [ HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ Confirmation ConfirmLogout ] "Logout"
                      ]
              , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal HideModal, HA.class' { back: true, hidden: model.smallScreen || model.user.temporary && SUR.temporaryUserExpiration model.user.joined <= Days 0.0 } ]
                      [ SIS.closeX []
                      ]
              ] <>
                    ( if model.user.temporary then [ temporaryUserSignUp model ] else []
                    )
            )
      where
      screenModal = case model.modal of
            Screen _ → true
            _ → false

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
temporaryUserSignUp { temporaryEmail, temporaryPassword, erroredFields, user: {  joined } } =
      HE.div [ HA.id $ show TemporaryUserSignUpForm ]
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