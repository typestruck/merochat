module Server.Elsewhere.Handler where

import Prelude

import Server.Elsewhere.Template as SET
import Server.Response as SR
import Server.Types (ServerEffect)
import Shared.Html (Html(..))

elsewhere ∷ ∀ r. { | r } → ServerEffect Html
elsewhere _ = SR.serveTemplate SET.template