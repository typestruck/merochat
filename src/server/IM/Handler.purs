module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Newtype as DN
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.R as SR

im :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect Html
im { guards: { loggedUserID } } = do
      user <- DN.unwrap <$> SID.presentUser loggedUserID
      suggestions <- SIA.suggest loggedUserID
      contacts <- SIA.contactList loggedUserID 0
      SR.serveTemplate $ SIT.template { contacts, suggestions, user }

contacts :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int } } -> ServerEffect (Array Contact)
contacts { guards: { loggedUserID }, query: { skip } } = SIA.contactList loggedUserID skip

--refactor: maybe contact instead of array contact
singleContact :: { guards :: { loggedUserID :: PrimaryKey }, query :: { id :: PrimaryKey } } -> ServerEffect (Array Contact)
singleContact { guards: { loggedUserID }, query: { id } } = DA.singleton <$> SIA.singleContact loggedUserID id

history :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int, with :: PrimaryKey } } -> ServerEffect (Array HistoryMessage)
history { guards: { loggedUserID }, query: { with, skip } } = SIA.history loggedUserID with skip