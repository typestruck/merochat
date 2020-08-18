module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.R as SR

im :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect Html
im { guards: { loggedUserID } } = do
      user <- SID.presentUser loggedUserID
      suggestions <- SIA.suggest loggedUserID
      contacts <- SIA.contactList loggedUserID 0
      SR.serveTemplate $ SIT.template { contacts, suggestions, user }