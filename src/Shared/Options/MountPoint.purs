module Shared.Options.MountPoint where

import Prelude

import Flame (AppId(..))
import Shared.Feedback.Types (FeedbackMessage)
import Shared.Im.Types (ImMessage)
import Shared.Profile.Types (ProfileMessage)

data MountPoint = Im | Profile | Feedback

profileId ∷ AppId MountPoint ProfileMessage
profileId = AppId Profile

imId ∷ AppId MountPoint ImMessage
imId = AppId Im

feedbackId ∷ AppId MountPoint FeedbackMessage
feedbackId = AppId Feedback

instance Show MountPoint where
      show = case _ of
            Im → "im-mount"
            Profile → "profile-mount"
            Feedback → "feedback-mount"
