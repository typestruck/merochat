module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Newtype as DN
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Response as SR

im :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect Html
im { guards: { loggedUserID } } = do
      user <- DN.unwrap <$> SID.presentUser loggedUserID
      suggestions <- SIA.suggest loggedUserID
      contacts <- SIA.listContacts loggedUserID 0
      SR.serveTemplate $ SIT.template { contacts, suggestions, user }

contacts :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int } } -> ServerEffect (Array Contact)
contacts { guards: { loggedUserID }, query: { skip } } = SIA.listContacts loggedUserID skip

--refactor: maybe contact instead of array contact
singleContact :: { guards :: { loggedUserID :: PrimaryKey }, query :: { id :: PrimaryKey } } -> ServerEffect (Array Contact)
singleContact { guards: { loggedUserID }, query: { id } } = DA.singleton <$> SIA.listSingleContact loggedUserID id

history :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int, with :: PrimaryKey } } -> ServerEffect (Array HistoryMessage)
history { guards: { loggedUserID }, query: { with, skip } } = SIA.resumeChatHistory loggedUserID with skip

suggestions :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect (Array Suggestion)
suggestions { guards: { loggedUserID } } = SIA.suggest loggedUserID

block :: { guards :: { loggedUserID :: PrimaryKey }, query :: { id :: PrimaryKey } } -> ServerEffect Ok
block { guards: { loggedUserID }, query: { id } } = SIA.blockUser loggedUserID id

missedMessages :: { guards :: { loggedUserID :: PrimaryKey }, query :: { since :: DateTimeWrapper } } -> ServerEffect (Array Contact)
missedMessages { guards: { loggedUserID }, query: { since: DateTimeWrapper since } } = SIA.listMissedContacts loggedUserID since