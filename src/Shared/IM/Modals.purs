module Shared.Im.View.Modals where

import Prelude
import Shared.ContentType
import Shared.Im.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Symbol as TDS
import Data.Tuple (Tuple)
import Environment (helpCSSHash, profileCSSHash, settingsCSSHash, backerCSSHash, experimentsCSSHash, leaderboardCSSHash)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Im.Types
import Shared.Im.Svg as SIA
import Shared.Im.View.Retry as SIVR
import Shared.Path as SP
import Type.Proxy (Proxy(..))

lazyLoad ∷  String → Html ImMessage
lazyLoad name = HE.link [ HA.rel "preload", HA.type' "text/css", HA.createAttribute "as" "style", HA.href $ SP.pathery CSS name, HA.createAttribute "onload" "this.onload=null;this.rel='stylesheet'" ]

modals ∷ ImModel → Html ImMessage
modals model@{ erroredFields, toggleModal } =
      HE.div (HA.class' { "modal-placeholder-overlay": true, "hidden": toggleModal == HideUserMenuModal })
            [ lazyLoad $ "help." <> helpCSSHash
            , lazyLoad $ "profile." <> profileCSSHash
            , lazyLoad $ "settings." <> settingsCSSHash
            , lazyLoad $ "profile." <> profileCSSHash
            , lazyLoad $ "backer." <> backerCSSHash
            , lazyLoad $ "experiments." <> experimentsCSSHash
            , lazyLoad $ "leaderboard." <> leaderboardCSSHash
            , case toggleModal of
                    ShowReport id → report id erroredFields
                    ConfirmLogout → confirmLogout
                    ConfirmDeleteChat tupleId → confirmDeleteChat tupleId
                    ConfirmBlockUser tupleId → confirmBlockUser tupleId
                    Tutorial step → tutorial step
                    _ → modalMenu model

            ]

report ∷ Int → Array String → Html ImMessage
report id erroredFields =
      HE.div (HA.class' "confirmation report")
            [ HE.span (HA.class' "report-title") "Report user"
            , HE.div (HA.class' "report-reasons") $ DA.mapWithIndex toRadio [ DatingContent, Harassment, HateSpeech, Spam, OtherReason ]
            , HE.span [ HA.class' { "error-message": true, "invisible": not (DA.elem (TDS.reflectSymbol (Proxy ∷ Proxy "reportReason")) erroredFields) } ] "Please choose a reason"
            , HE.div (HA.class' "report-comment")
                    [ HE.label_ "Comment"
                    , HE.input [ HA.type' "text", HA.maxlength 300, HA.class' "modal-input", HA.onInput setReportComment ]
                    ]
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal ] "Cancel"
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
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick Logout ] "Logout"
                    ]
            ]

confirmDeleteChat ∷ Tuple Int (Maybe Int) → Html ImMessage
confirmDeleteChat tupleId =
      HE.div (HA.class' "confirmation")
            [ HE.span (HA.class' "bold") "Do you really want to delete this chat?"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ DeleteChat tupleId ] "Delete"
                    ]
            ]

confirmBlockUser ∷ Tuple Int (Maybe Int) → Html ImMessage
confirmBlockUser tupleId =
      HE.div (HA.class' "confirmation")
            [ HE.span (HA.class' "bold") "Do you really want to block this user?"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal ] "Cancel"
                    , HE.button [ HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ BlockUser tupleId ] "Block"
                    ]
            ]

tutorial ∷ Step → Html ImMessage
tutorial = case _ of
      Welcome → HE.div (HA.class' "confirmation tutorial")
            [ HE.span (HA.class' "bold") "Welcome!"
            , HE.span_ "Let's take you through a brief tutorial"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial ChatSuggestions ] "Start!"
                    ]
            ]
      ChatSuggestions → HE.div (HA.class' "confirmation tutorial ")
            [ HE.span (HA.class' "bold") "Chat suggestions",
                  HE.span_ "Use the arrows to move back and forth all your suggestions"
            ,  HE.span_ "When you see someone you'd like to chat with, send them a message to continue"
            , HE.div (HA.class' "buttons")
                    [ HE.button [ HA.class' "green-button step-button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Tutorial ChatSuggestions, HA.disabled true ] "Done!"
                    ]
            ]

modalMenu ∷ ImModel → Html ImMessage
modalMenu { toggleModal, failedRequests } =
      HE.div (HA.class' "modal-placeholder")
            [ HE.div (HA.class' "modal-menu-mobile")
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal ]
                    , HE.strong_ $ show toggleModal
                    ]
            , HE.div (HA.class' "modal-menu")
                    [ HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal, HA.class' "back" ]
                            [ HE.svg [ HA.class' "svg-16", HA.viewBox "0 0 30 30" ]
                                    [ HE.path' [ HA.d "M30 13.125H7.18125L17.6625 2.64375L15 0L0 15L15 30L17.6437 27.3563L7.18125 16.875H30V13.125Z" ]
                                    ]
                            , HE.text " Back to chats"
                            ]
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.class' { entry: true, selected: toggleModal == ShowProfile } ] $ show ShowProfile
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings, HA.class' { entry: true, selected: toggleModal == ShowSettings } ] $ show ShowSettings
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowBacker, HA.class' { entry: true, selected: toggleModal == ShowBacker } ] $ show ShowBacker
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments, HA.class' { entry: true, selected: toggleModal == ShowExperiments } ] $ show ShowExperiments
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard, HA.class' { entry: true, selected: toggleModal == ShowLeaderboard } ] $ show ShowLeaderboard
                    , HE.div [ HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp, HA.class' { entry: true, selected: toggleModal == ShowHelp } ] $ show ShowHelp
                    ]
            , HE.div [ HA.id "profile-edition-root", HA.class' { hidden: toggleModal /= ShowProfile } ] $ retry ShowProfile
            , HE.div [ HA.id "settings-edition-root", HA.class' { hidden: toggleModal /= ShowSettings } ] $ retry ShowSettings
            , HE.div [ HA.id "backer-root", HA.class' { hidden: toggleModal /= ShowBacker } ] $ retry ShowBacker
            , HE.div [ HA.id "experiments-root", HA.class' { hidden: toggleModal /= ShowExperiments } ] $ retry ShowExperiments
            , HE.div [ HA.id "karma-leaderboard-root", HA.class' { hidden: toggleModal /= ShowLeaderboard } ] $ retry ShowLeaderboard
            , HE.div [ HA.id "help-root", HA.class' { hidden: toggleModal /= ShowHelp } ] $ retry ShowHelp
            ]
      where
            retry tm = HE.div (HA.class' "retry-modal")
                  [ SIVR.retry "Failed to load contents" (ToggleModal tm) failedRequests
                  , HE.div' (HA.class' "loading")
                  ]