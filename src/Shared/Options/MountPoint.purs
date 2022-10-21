module Shared.Options.MountPoint where

import Prelude

import Flame (AppId(..))
import Shared.Experiments.Types (ChatExperimentMessage)
import Shared.Feedback.Types (FeedbackMessage)
import Shared.Im.Types (ImMessage)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage)
import Shared.Profile.Types (ProfileMessage)

data MountPoint = Im | Profile | Feedback | Experiments | KarmaPrivileges

profileId ∷ AppId MountPoint ProfileMessage
profileId = AppId Profile

experimentsId ∷ AppId MountPoint ChatExperimentMessage
experimentsId = AppId Experiments

imId ∷ AppId MountPoint ImMessage
imId = AppId Im

karmaPrivilegesId ∷ AppId MountPoint KarmaPrivilegesMessage
karmaPrivilegesId = AppId KarmaPrivileges

feedbackId ∷ AppId MountPoint FeedbackMessage
feedbackId = AppId Feedback

instance Show MountPoint where
      show = case _ of
            Im → "im-mount"
            Profile → "profile-mount"
            Feedback → "feedback-mount"
            Experiments → "experiments-mount"
            KarmaPrivileges → "karma-privileges-mount"
