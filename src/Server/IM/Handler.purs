module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Run as R
import Run.Except as RE
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Response as SR

im :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect (Response String)
im { guards: { loggedUserID } } = do
      maybeUser <- SID.presentUser loggedUserID
      case maybeUser of
            --nothing can only happen in case the user has an invalid cookie
            Nothing -> RE.throw ExpiredSession
            Just user -> do
                  suggestions <- SIA.suggest loggedUserID 0
                  contacts <- SIA.listContacts loggedUserID 0
                  contents <- R.liftEffect $ SIT.template { contacts, suggestions, user: DN.unwrap user }
                  pure <<< PSR.setHeaders (PH.fromFoldable [Tuple "content-type" html, Tuple "cache-control" "no-store, max-age=0"]) $ PSR.ok contents

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

missedEvents :: { guards :: { loggedUserID :: PrimaryKey }, query :: { lastSenderID :: Maybe Int, lastRecipientID :: Maybe Int } } -> ServerEffect MissedEvents
missedEvents { guards: { loggedUserID }, query: { lastSenderID, lastRecipientID } } = SIA.listMissedEvents loggedUserID lastSenderID lastRecipientID