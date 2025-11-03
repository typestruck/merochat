module Server.InternalBacker.Handler where

import Server.Effect

import Run as R
import Server.InternalBacker.Template as SIBT
import Shared.Html (Html(..))

internalBacker ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
internalBacker _ = R.liftEffect SIBT.template
