module Shared.Element where

import Prelude

import Data.Hashable (class Hashable)
import Data.Hashable as HS

data ElementId
      = UserContextMenu
      | SuggestionContextMenu
      | CompactProfileContextMenu
      | FullProfileContextMenu
      | ImageFileInput
      | MiniSuggestionContextMenu
      | ChatInputContact
      | ChatInputSuggestion
      | ChatInput
      | ProfileEditionForm
      | ImageFormCaption
      | PasswordDiv
      | FeedbackForm
      | ConfirmPasswordInput
      | Im
      | MiniChatInputSuggestion
      | LinkFormUrl
      | MessageHistory
      | ChatInputBigSuggestion
      | Cards
      | Favicon
      | ProfileEditionRoot
      | ChatInputPreview
      | SettingsEditionRoot
      | ExperimentsRoot
      | HelpRoot
      | CaptchaTemporaryUser
      | TermsLink
      | PrivacyLink
      | Faq
      | TermsSection
      | PasswordInput
      | EmailDiv
      | PrivacySection
      | EmailInput
      | BackerRoot
      | ConfirmPassword
      | FeedbackRoot
      | ScreenshotInput
      | FaqLink
      | CaptchaRegularUser
      | TemporaryUserSignUp
      | ConfirmAccountTerminationForm
      | AvatarFileInput
      | TemporaryUserSignUpForm
      | KarmaPrivilegesRoot

instance Hashable ElementId where
      hash = HS.hash <<< show

instance Show ElementId where
      show = case _ of
            UserContextMenu → "user-context-menu"
            SuggestionContextMenu → "suggestion-context-menu"
            CompactProfileContextMenu → "compact-profile-context-menu"
            FullProfileContextMenu → "full-profile-context-menu"
            ImageFileInput → "image-file-input"
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
            ConfirmPasswordInput → "confirm-password-input"
            PasswordDiv → "password"
            CaptchaRegularUser → "captcha-regular-user"
            CaptchaTemporaryUser → "captcha-temporary-user"
            TermsLink → "terms-link"
            PrivacyLink → "privacy-link"
            Faq → "faq"
            TermsSection → "terms"
            Im → "im"
            EmailDiv → "email"
            EmailInput → "email-input"
            PrivacySection → "privacy"
            ConfirmPassword → "confirm-password"
            FaqLink → "faq-link"
            BackerRoot → "backer-root"
            TemporaryUserSignUp → "temporary-user-sign-up"
            TemporaryUserSignUpForm → "temporary-user-sign-up-form"
            ChatInputPreview → "chat-input-preview"
            ProfileEditionRoot → "profile-edition-root"
            SettingsEditionRoot → "settings-edition-root"
            HelpRoot → "help-root"
            ExperimentsRoot → "experiments-root"
            PasswordInput → "password-input"
            KarmaPrivilegesRoot → "karma-privileges-root"
            AvatarFileInput → "avatar-file-input"

derive instance Eq ElementId