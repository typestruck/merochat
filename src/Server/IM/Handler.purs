module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.ContentType
import Shared.IM.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Run.Except as RE
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Database.Flat (fromFlatUser)
import Server.IM.Template as SIT
import Server.Ok (Ok)
import Server.Response as SR
import Shared.ResponseError (ResponseError(..))

im ∷ { guards ∷ { loggedUserID ∷ Int } } → ServerEffect (Response String)
im { guards: { loggedUserID } } = do
      maybeUser ← SID.presentUser loggedUserID
      case maybeUser of
            --happens if the user has an invalid cookie/was suspended
            Nothing → RE.throw ExpiredSession
            Just user → do
                  suggestions ← SIA.suggest loggedUserID 0 Nothing
                  contacts ← SIA.listContacts loggedUserID 0
                  Html contents ← SR.serveTemplate $ SIT.template { contacts, suggestions, user: fromFlatUser user }
                  pure <<< PSR.setHeaders (PH.fromFoldable [ Tuple "content-type" html, Tuple "cache-control" "no-store, max-age=0" ]) $ PSR.ok contents

contacts ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { skip ∷ Int } } → ServerEffect (Array Contact)
contacts { guards: { loggedUserID }, query: { skip } } = SIA.listContacts loggedUserID skip

--not sure if a bug, but payload has no DecodeResponse instance for Maybe, and sending one results in a runtime exception
singleContact ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { id ∷ Int, contactsOnly ∷ Boolean } } → ServerEffect (Array Contact)
singleContact { guards: { loggedUserID }, query: { id, contactsOnly } } = SIA.listSingleContact loggedUserID id contactsOnly

history ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { skip ∷ Int, with ∷ Int } } → ServerEffect (Array HistoryMessage)
history { guards: { loggedUserID }, query: { with, skip } } = SIA.resumeChatHistory loggedUserID with skip

suggestions ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { skip ∷ Int, avoid ∷ Maybe ArrayPrimaryKey } } → ServerEffect (Array Suggestion)
suggestions { guards: { loggedUserID }, query: { skip, avoid } } = SIA.suggest loggedUserID skip avoid

block ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ { id ∷ Int } } → ServerEffect Ok
block { guards: { loggedUserID }, body: { id } } = SIA.blockUser loggedUserID id

deleteChat ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ { userId ∷ Int, messageId ∷ Int } } → ServerEffect Ok
deleteChat { guards: { loggedUserID }, body } = SIA.deleteChat loggedUserID body

missedEvents ∷ { guards ∷ { loggedUserID ∷ Int }, query ∷ { lastSenderID ∷ Maybe Int, lastRecipientId ∷ Maybe Int } } → ServerEffect MissedEvents
missedEvents { guards: { loggedUserID }, query: { lastSenderID, lastRecipientId } } = SIA.listMissedEvents loggedUserID lastSenderID lastRecipientId

report ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ Report } → ServerEffect Ok
report { guards: { loggedUserID }, body } = SIA.reportUser loggedUserID body