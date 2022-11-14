module Server.Help.Handler where

import Prelude
import Server.Effect

import Server.Help.Template as SHT
import Server.Response as SR
import Shared.Html (Html)

help ∷ ∀ r. { | r } → ServerEffect Html
help _ = SR.serveTemplate $ SHT.template
