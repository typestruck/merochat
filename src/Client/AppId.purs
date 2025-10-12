module Client.AppId where

import Prelude

import Flame (AppId(..))
import Shared.Experiments.Types (ExperimentsMessage)
import Shared.Feedback.Types (FeedbackMessage)
import Shared.Im.Types (ImMessage)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage)
import Shared.Profile.Types (ProfileMessage)

data ClientAppId
      = Im
      | Profile
      | Feedback
      | Experiments
      | KarmaPrivileges

profileAppId ∷ AppId ClientAppId ProfileMessage
profileAppId = AppId Profile

experimentsAppId ∷ AppId ClientAppId ExperimentsMessage
experimentsAppId = AppId Experiments

imAppId ∷ AppId ClientAppId ImMessage
imAppId = AppId Im

karmaPrivilegesAppId ∷ AppId ClientAppId KarmaPrivilegesMessage
karmaPrivilegesAppId = AppId KarmaPrivileges

feedbackAppId ∷ AppId ClientAppId FeedbackMessage
feedbackAppId = AppId Feedback

instance Show ClientAppId where
      show = case _ of
            Im → "im-app-id"
            Profile → "profile-app-id"
            Feedback → "feedback-app-id"
            Experiments → "experiments-app-id"
            KarmaPrivileges → "karma-privileges-app-id"
