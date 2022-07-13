module Server.IM.Action where

import Prelude
import Server.Ok

import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Droplet.Driver (Pool)
import Server.Email as SE
import Server.File as SF
import Server.IM.Database as SID
import Server.IM.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.IM.Database.Flat as SIF
import Server.Types (BaseEffect, ServerEffect, Configuration)
import Server.Wheel as SW
import Shared.IM.Types (ArrayPrimaryKey, Contact, HistoryMessage, MessageContent(..), MissedEvents, Report, Suggestion, Turn)
import Shared.Options.File (imageBasePath)

foreign import sanitize ∷ String → String

suggest ∷ Int → Int → Maybe ArrayPrimaryKey → ServerEffect (Array Suggestion)
suggest loggedUserId skip keys = map SIF.fromFlatUser <$> SID.suggest loggedUserId skip keys

listContacts ∷ Int → Int → ServerEffect (Array Contact)
listContacts loggedUserId skip = presentContacts <$> SID.presentContacts loggedUserId skip

listSingleContact ∷ Int → Int → Boolean → ServerEffect (Array Contact)
listSingleContact loggedUserId userId contactsOnly = presentContacts <$> SID.presentSingleContact loggedUserId userId 0

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

processMessage ∷ ∀ r. Int → Int → Int → MessageContent → BaseEffect { configuration ∷ Configuration, pool ∷ Pool | r } (Tuple Int String)
processMessage loggedUserId userId temporaryId content = do
      sanitized ← processMessageContent content
      id ← SID.insertMessage loggedUserId userId temporaryId sanitized
      pure $ Tuple id sanitized

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

blockUser ∷ Int → Int → ServerEffect Ok
blockUser loggedUserId userId = do
      SID.insertBlock loggedUserId userId
      pure ok

deleteChat ∷ Int → { userId ∷ Int, messageId ∷ Int } → ServerEffect Ok
deleteChat loggedUserId ids@{ userId } = do
      entry ← SID.chatHistoryEntry loggedUserId userId
      case entry of
            Nothing → pure ok
            Just { sender } → do
                  SID.markAsDeleted (sender == loggedUserId) loggedUserId ids
                  pure ok

reportUser ∷ Int → Report → ServerEffect Ok
reportUser loggedUserId report@{ reason, userId } = do
      SID.insertReport loggedUserId report
      SE.sendEmail "contact@melan.chat" ("[REPORT] " <> show reason) $ "select * from reports where reported = " <> show userId <> ";"
      pure ok