module Shared.Element
  ( ElementId(..)
  , toQuerySelector
  )
  where

import Prelude

import Data.Hashable (class Hashable)
import Data.Hashable as HS
import Web.DOM.ParentNode (QuerySelector(..))

data ElementId
      = SuggestionContextMenu
      | CompactProfileContextMenu
      | FullProfileContextMenu
      | ImageFileInput
      | MiniSuggestionContextMenu
      | ChatInputContact
      | ChatInputSuggestion
      | ChatInput
      | ProfileEditionForm
      | ImageFormCaption
      | FeedbackForm
      | PasswordConfirmationInput
      | Im
      | MiniChatInputSuggestion
      | LinkFormUrl
      | MessageHistory
      | ChatInputBigSuggestion
      | Cards
      | Favicon
      | ProfileEditionRoot
      | SettingsEditionRoot
      | ExperimentsRoot
      | HelpRoot
      | TermsLink
      | PrivacyLink
      | Faq
      | TermsSection
      | PasswordInput
      | PrivacySection
      | EmailInput
      | BackerRoot
      | FeedbackRoot
      | ScreenshotInput
      | FaqLink
      | CaptchaUser
      | TemporaryUserSignUp
      | CreateAccount
      | ConfirmAccountTerminationForm
      | AvatarFileInput
      | TemporaryUserSignUpForm
      | KarmaPrivilegesRoot

toQuerySelector :: ElementId -> QuerySelector
toQuerySelector id = QuerySelector $ "#" <> show id

instance Hashable ElementId where
      hash = HS.hash <<< show

instance Show ElementId where
      show = case _ of
            SuggestionContextMenu → "suggestion-context-menu"
            CompactProfileContextMenu → "compact-profile-context-menu"
            FullProfileContextMenu → "full-profile-context-menu"
            ImageFileInput → "image-file-input"
            CreateAccount → "create-account"
            ConfirmAccountTerminationForm → "confirm-account-termination-form"
            LinkFormUrl → "link-form-url"
            ProfileEditionForm → "profile-edition-form"
            MiniSuggestionContextMenu → "mini-suggestion-context-menu"
            Cards → "cards"
            ChatInput → "chat-input"
            ScreenshotInput → "screenshot-input"
            ChatInputSuggestion → "chat-input-suggestion"
            ChatInputContact → "chat-input-contact"
            MiniChatInputSuggestion → "mini-chat-input-suggestion"
            ImageFormCaption → "image-form-caption"
            FeedbackForm → "feedback-form"
            ChatInputBigSuggestion → "chat-input-big-suggestion"
            MessageHistory → "message-history"
            Favicon → "favicon"
            FeedbackRoot → "feedback-root"
            PasswordConfirmationInput → "password-confirmation-input"
            CaptchaUser → "captcha-user"
            TermsLink → "terms-link"
            PrivacyLink → "privacy-link"
            Faq → "faq"
            TermsSection → "terms"
            Im → "im"
            EmailInput → "email-input"
            PrivacySection → "privacy"
            FaqLink → "faq-link"
            BackerRoot → "backer-root"
            TemporaryUserSignUp → "temporary-user-sign-up"
            TemporaryUserSignUpForm → "temporary-user-sign-up-form"
            ProfileEditionRoot → "profile-edition-root"
            SettingsEditionRoot → "settings-edition-root"
            HelpRoot → "help-root"
            ExperimentsRoot → "experiments-root"
            PasswordInput → "password-input"
            KarmaPrivilegesRoot → "karma-privileges-root"
            AvatarFileInput → "avatar-file-input"

derive instance Eq ElementId