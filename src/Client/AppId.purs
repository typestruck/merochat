module Client.AppId where

import Prelude

import Flame (AppId(..))
import Shared.Experiments.Types (ExperimentsMessage)
import Shared.Feedback.Types (FeedbackMessage)
import Shared.Im.Types (ImMessage)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage)
import Shared.Profile.Types (ProfileMessage)
import Shared.Settings.Types (SettingsMessage)
import Data.Void (Void)
import Shared.Login.Types (LoginMessage)
import Shared.Landing.Types (LandingMessage)
import Shared.Recover.Types (RecoverMessage)
import Shared.Backer.Types (BackerMessage)

data ClientAppId
      = Im
      | Login
      | Landing
      | Profile
      | Settings
      | Feedback
      | Experiments
      | KarmaPrivileges
      | Help
      | Terms
      | Privacy
      | Backer
      | Recover
      | Banned

profileAppId ∷ AppId ClientAppId ProfileMessage
profileAppId = AppId Profile

experimentsAppId ∷ AppId ClientAppId ExperimentsMessage
experimentsAppId = AppId Experiments

imAppId ∷ AppId ClientAppId ImMessage
imAppId = AppId Im

loginAppId ∷ AppId ClientAppId LoginMessage
loginAppId = AppId Login

landingAppId ∷ AppId ClientAppId LandingMessage
landingAppId = AppId Landing

karmaPrivilegesAppId ∷ AppId ClientAppId KarmaPrivilegesMessage
karmaPrivilegesAppId = AppId KarmaPrivileges

feedbackAppId ∷ AppId ClientAppId FeedbackMessage
feedbackAppId = AppId Feedback

settingsAppId ∷ AppId ClientAppId SettingsMessage
settingsAppId = AppId Settings

helpAppId ∷ AppId ClientAppId Void
helpAppId = AppId Help

termsAppId ∷ AppId ClientAppId Void
termsAppId = AppId Terms

privacyAppId ∷ AppId ClientAppId Void
privacyAppId = AppId Privacy

backerAppId ∷ AppId ClientAppId BackerMessage
backerAppId = AppId Backer

recoverAppId ∷ AppId ClientAppId RecoverMessage
recoverAppId = AppId Recover

bannedAppId ∷ AppId ClientAppId Void
bannedAppId = AppId Banned

instance Show ClientAppId where
      show = case _ of
            Im → "im-app-id"
            Login → "login-app-id"
            Landing → "landing-app-id"
            Profile → "profile-app-id"
            Settings → "settings-app-id"
            Feedback → "feedback-app-id"
            Experiments → "experiments-app-id"
            KarmaPrivileges → "karma-privileges-app-id"
            Help → "help-app-id"
            Terms → "terms-app-id"
            Privacy → "privacy-app-id"
            Backer → "backer-app-id"
            Recover → "recover-app-id"
            Banned → "banned-app-id"
