module Server.Unsubscribe.Handler where

import Prelude
import Server.Effect (ServerEffect)

import Server.NotFound.Template as SUN
import Server.Response as SR
import Server.Unsubscribe.Action as SUA
import Server.Unsubscribe.Template as SUT
import Shared.Html (Html)

unsubscribe ∷ { query ∷ { emailId ∷ String } } → ServerEffect Html
unsubscribe { query: { emailId } } = do
      unsubbed ← SUA.unsubscribe emailId
      SR.serveTemplate $ if unsubbed then SUT.template else SUN.template
