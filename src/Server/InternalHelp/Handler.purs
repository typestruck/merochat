module Server.InternalHelp.Handler where

import Server.Effect

import Run as R
import Server.InternalHelp.Template as SIHT
import Shared.Html (Html)

internalHelp ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
internalHelp _ = R.liftEffect SIHT.template
