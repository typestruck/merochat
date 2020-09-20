module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Run as R
import Run.Except as RE
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Response as SR

im :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect Html
im { guards: { loggedUserID } } = do
      void $ R.liftEffect $ throw "this is hte errr"
      maybeUser <- SID.presentUser loggedUserID
      case maybeUser of
            --nothing can only happen in case the user has an invalid cookie
            Nothing -> RE.throw ExpiredSession
            Just user -> do
                  suggestions <- SIA.suggest loggedUserID 0
                  contacts <- SIA.listContacts loggedUserID 0
                  SR.serveTemplate $ SIT.template { contacts, suggestions, user: DN.unwrap user }

contacts :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int } } -> ServerEffect (Array Contact)
contacts { guards: { loggedUserID }, query: { skip } } = SIA.listContacts loggedUserID skip

--refactor: maybe contact instead of array contact
singleContact :: { guards :: { loggedUserID :: PrimaryKey }, query :: { id :: PrimaryKey } } -> ServerEffect (Array Contact)
singleContact { guards: { loggedUserID }, query: { id } } = DA.singleton <$> SIA.listSingleContact loggedUserID id

history :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int, with :: PrimaryKey } } -> ServerEffect (Array HistoryMessage)
history { guards: { loggedUserID }, query: { with, skip } } = SIA.resumeChatHistory loggedUserID with skip

suggestions :: { guards :: { loggedUserID :: PrimaryKey }, query :: { skip :: Int } } -> ServerEffect (Array Suggestion)
suggestions { guards: { loggedUserID }, query: { skip } } = SIA.suggest loggedUserID skip

block :: { guards :: { loggedUserID :: PrimaryKey }, query :: { id :: PrimaryKey } } -> ServerEffect Ok
block { guards: { loggedUserID }, query: { id } } = SIA.blockUser loggedUserID id

missedMessages :: { guards :: { loggedUserID :: PrimaryKey }, query :: { lastID :: Int } } -> ServerEffect (Array Contact)
missedMessages { guards: { loggedUserID }, query: { lastID } } = SIA.listMissedContacts loggedUserID lastID