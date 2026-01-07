module Server.Im.Handler where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe)

import Data.Tuple (Tuple(..))
import Effect.Class as EC
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Im.Action as SIA
import Server.Im.Template as SIT
import Shared.Account (EmailPassword)
import Shared.Changelog (Changelog)
import Shared.Html (Html)

im ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Response Html)
im { guards: { loggedUserId } } = do
      payload ← SIA.im loggedUserId
      contents ← EC.liftEffect $ SIT.template payload
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

react ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int, reaction ∷ String } } → ServerEffect Empty
react routes = do
      SIA.react routes.guards.loggedUserId routes.body.id routes.body.reaction
      pure Empty

favorite ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int } } → ServerEffect Empty
favorite routes = do
      SIA.favorite routes.guards.loggedUserId routes.body.userId
      pure Empty

block ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int } } → ServerEffect Empty
block { guards: { loggedUserId }, body: { id } } = do
      SIA.blockUser loggedUserId id
      pure Empty

deleteChat ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int, messageId ∷ Int } } → ServerEffect Empty
deleteChat { guards: { loggedUserId }, body } = do
      SIA.deleteChat loggedUserId body
      pure Empty

subscribe ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Empty
subscribe { guards } = do
      SIA.subscribe guards.loggedUserId
      pure Empty

changelogs ∷ { query ∷ { before ∷ Maybe Int }, guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Changelog)
changelogs routes = SIA.listChangelogs routes.guards.loggedUserId routes.query.before

changelog ∷ { body ∷ { ids ∷ Array Int }, guards ∷ { loggedUserId ∷ Int } } → ServerEffect Empty
changelog routes = do
      SIA.markRead routes.guards.loggedUserId routes.body.ids
      pure Empty

missedContacts ∷ { query ∷ { since ∷ DateTime, last ∷ Maybe Int }, guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Contact)
missedContacts routes = SIA.listMissedContacts routes.guards.loggedUserId routes.query.since routes.query.last

report ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ Report } → ServerEffect Empty
report { guards: { loggedUserId }, body } = do
      SIA.reportUser loggedUserId body
      pure Empty

tutorial ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Empty
tutorial { guards: { loggedUserId } } = do
      SIA.finishTutorial loggedUserId
      pure Empty

greeting ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Empty
greeting { guards: { loggedUserId } } = do
      SIA.greet loggedUserId
      pure Empty

register ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ EmailPassword } → ServerEffect Empty
register { guards: { loggedUserId }, body: { email, password } } = do
      SIA.registerUser loggedUserId email password
      pure Empty
