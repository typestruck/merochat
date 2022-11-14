module Server.Banned.Handler where

import Prelude

import Server.Banned.Template as SBT
import Server.Response as SR
import Server.Effect (ServerEffect)
import Shared.Html (Html(..))

banned ∷ ∀ r. { | r } → ServerEffect Html
banned _ = SR.serveTemplate SBT.template