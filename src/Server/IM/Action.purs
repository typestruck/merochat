module Server.IM.Action where

import Debug
import Prelude

import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Droplet.Driver (Pool)
import Run.Except as RE
import Server.Email as SE
import Server.File as SF
import Server.IM.Database as SID
import Server.IM.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.IM.Database.Flat as SIF
import Server.Types (BaseEffect, ServerEffect, Configuration)
import Server.Wheel as SW
import Shared.IM.Types (ArrayPrimaryKey, Contact, HistoryMessage, MessageContent(..), MissedEvents, Report, Suggestion, Turn)
import Shared.Options.File (imageBasePath)
import Shared.ResponseError (ResponseError(..))
import Server.IM.Types (Payload)

foreign import sanitize ∷ String → String

im ∷ Int → ServerEffect Payload
im loggedUserId = do
      maybeUser ← SID.presentUser loggedUserId
      case maybeUser of
            --happens if the user has an invalid cookie/was suspended
            Nothing → RE.throw ExpiredSession
            Just user → do
                  suggestions ← suggest loggedUserId 0 Nothing
                  contacts ← listContacts loggedUserId 0
                  pure {contacts, suggestions, user: SIF.fromFlatUser user}

suggest ∷ Int → Int → Maybe ArrayPrimaryKey → ServerEffect (Array Suggestion)
suggest loggedUserId skip keys = map SIF.fromFlatUser <$> SID.suggest loggedUserId skip keys

listContacts ∷ Int → Int → ServerEffect (Array Contact)
listContacts loggedUserId skip = presentContacts <$> SID.presentContacts loggedUserId skip

listSingleContact ∷ Int → Int → ServerEffect (Array Contact)
listSingleContact loggedUserId userId = presentContacts <$> SID.presentSingleContact loggedUserId userId 0

resumeChatHistory ∷ Int → Int → Int → ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserId userId skip = map fromFlatMessage <$> SID.presentSingleContact loggedUserId userId skip

listMissedEvents ∷ Int → Maybe Int → Maybe Int → ServerEffect MissedEvents
listMissedEvents loggedUserId lastSenderId lastRecipientId = do
      messageIds ← DM.maybe (pure []) (SID.messageIdsFor loggedUserId) lastSenderId
      contacts ← DM.maybe (pure []) (SID.presentMissedContacts loggedUserId) lastRecipientId
      pure
            { contacts: presentContacts contacts
            , messageIds
            }

presentContacts ∷ Array FlatContactHistoryMessage → Array Contact
presentContacts = map chatHistory <<< DA.groupBy sameContact
      where
      sameContact a b = a.id == b.id

      chatHistory records =
            let contact = DAN.head records in (fromFlatContact contact) { history = fromFlatMessage <$> DAN.toArray records }

processMessage ∷ ∀ r. Int → Int → Int → MessageContent → BaseEffect { configuration ∷ Configuration, pool ∷ Pool | r } (Maybe (Tuple Int String))
processMessage loggedUserId userId temporaryId content = do
      isVisible ← SID.isRecipientVisible loggedUserId userId
      if isVisible then do
            sanitized ← processMessageContent content
            id ← SID.insertMessage loggedUserId userId temporaryId sanitized
            pure <<< Just $ Tuple id sanitized
      else pure Nothing

-- | Sanitizes markdown and handle image uploads
processMessageContent ∷ ∀ r. MessageContent → BaseEffect { configuration ∷ Configuration, pool ∷ Pool | r } String
processMessageContent content = do
      message ← case content of
            Text m → pure m
            Image caption base64 → do
                  path ← SF.saveBase64File $ base64
                  pure $ "![" <> caption <> "](" <> imageBasePath <> "upload/" <> path <> ")"
      pure <<< DS.trim $ sanitize message

processKarma ∷ ∀ r. Int → Int → Turn → BaseEffect { pool ∷ Pool | r } Unit
processKarma loggedUserId userId turn = SID.insertKarma loggedUserId userId $ SW.karmaFrom turn

blockUser ∷ Int → Int → ServerEffect Unit
blockUser loggedUserId userId = SID.insertBlock loggedUserId userId

deleteChat ∷ Int → { userId ∷ Int, messageId ∷ Int } → ServerEffect Unit
deleteChat loggedUserId ids@{ userId } = do
      entry ← SID.chatHistoryEntry loggedUserId userId
      case entry of
            Nothing → pure unit
            Just { sender } → SID.markAsDeleted (sender == loggedUserId) loggedUserId ids

reportUser ∷ Int → Report → ServerEffect Unit
reportUser loggedUserId report@{ reason, userId } = do
      SID.insertReport loggedUserId report
      SE.sendEmail "contact@melan.chat" ("[REPORT] " <> show reason) $ "select * from reports where reported = " <> show userId <> ";"