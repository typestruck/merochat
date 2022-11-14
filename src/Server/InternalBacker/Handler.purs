module Server.InternalBacker.Handler where

import Server.Effect
import Server.InternalBacker.Template as SIBT
import Run as R

internalBacker ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
internalBacker _ = R.liftEffect SIBT.template
