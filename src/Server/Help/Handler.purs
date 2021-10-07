module Server.Help.Handler where

import Prelude
import Server.Types

import Server.Help.Template as SHT
import Server.Response as SR
import Shared.ContentType (Html(..))

help ∷ ∀ r. { | r } → ServerEffect Html
help _ = SR.serveTemplate $ SHT.template
