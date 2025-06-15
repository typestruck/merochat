module Server.Im.Handler where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.Maybe (Maybe)
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Im.Action as SIA
import Server.Im.Template as SIT
import Server.Ok (Ok, ok)
import Server.Response as SR
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Html (Html(..))

im ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Response String)
im { guards: { loggedUserId } } = do
      payload ← SIA.im loggedUserId
      Html contents ← SR.serveTemplate $ SIT.template payload
      pure <<< PSR.setHeaders (PH.fromFoldable [ Tuple "content-type" html, Tuple "cache-control" "no-store, max-age=0" ]) $ PSR.ok contents

contacts ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { skip ∷ Int } } → ServerEffect (Array Contact)
contacts { guards: { loggedUserId }, query: { skip } } = SIA.listContacts loggedUserId skip

--not sure if a bug, but payload has no DecodeResponse instance for Maybe, and sending one results in a runtime exception
contact ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { id ∷ Int } } → ServerEffect (Array Contact)
contact { guards: { loggedUserId }, query: { id } } = SIA.listSingleContact loggedUserId id

history ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { skip ∷ Int, with ∷ Int } } → ServerEffect (Array HistoryMessage)
history { guards: { loggedUserId }, query: { with, skip } } = SIA.resumeChatHistory loggedUserId with skip

suggestions ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { skip ∷ Int, sg ∷ SuggestionsFrom } } → ServerEffect (Array Suggestion)
suggestions { guards: { loggedUserId }, query } = SIA.suggest loggedUserId query.skip query.sg

block ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int } } → ServerEffect Ok
block { guards: { loggedUserId }, body: { id } } = do
      SIA.blockUser loggedUserId id
      pure ok

deleteChat ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int, messageId ∷ Int } } → ServerEffect Ok
deleteChat { guards: { loggedUserId }, body } = do
      SIA.deleteChat loggedUserId body
      pure ok

missedContacts ∷ { query ∷ { since ∷ DateTimeWrapper, last :: Maybe Int }, guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Contact)
missedContacts request = SIA.listMissedContacts request.guards.loggedUserId request.query.since request.query.last

report ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ Report } → ServerEffect Ok
report { guards: { loggedUserId }, body } = do
      SIA.reportUser loggedUserId body
      pure ok

tutorial ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Ok
tutorial { guards: { loggedUserId } } = do
      SIA.finishTutorial loggedUserId
      pure ok

greeting ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Ok
greeting { guards: { loggedUserId } } = do
      SIA.greet loggedUserId
      pure ok

register ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { email ∷ String, password ∷ String } } → ServerEffect Ok
register { guards: { loggedUserId }, body: { email, password } } = do
      SIA.registerUser loggedUserId email password
      pure ok
