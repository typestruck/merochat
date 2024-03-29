module Server.InternalHelp.Handler where

import Server.Effect
import Server.InternalHelp.Template as SIHT
import Run as R

internalHelp ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
internalHelp _ = R.liftEffect SIHT.template
