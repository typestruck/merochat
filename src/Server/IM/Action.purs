--refactor: here and other modueles only export whats needed
module Server.IM.Action where

import Prelude

import Data.Array as DA
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Droplet.Driver (Pool)
import Effect.Ref (Ref)
import Server.Email as SE
import Server.File as SF
import Server.IM.Database as SID
import Server.IM.Flat (fromFlatContact)
import Server.IM.Flat as SIF
import Server.Ok (ok)
import Server.Types (BaseEffect, Ok, ServerEffect, StorageDetails)
import Server.Wheel as SW
import Shared.IM.Types (Contact, HistoryMessage, MessageContent(..), MissedEvents, Suggestion, Turn)
import Shared.Options.File (imageBasePath)
import Shared.Types (ArrayPrimaryKey, Report)
import Shared.Unsafe as SU

foreign import sanitize ∷ String → String

suggest ∷ Int → Int → Maybe ArrayPrimaryKey → ServerEffect (Array Suggestion)
suggest loggedUserID skip keys = map SIF.fromFlatUser <$> SID.suggest loggedUserID skip keys

listContacts ∷ Int → Int → ServerEffect (Array Contact)
listContacts loggedUserID skip = do
      contacts ← (fromFlatContact <$> _) <$> SID.presentContacts loggedUserID skip
      history ← SID.chatHistoryFor loggedUserID $ map (_.id <<< _.user) contacts
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where
      intoHashMap hashMap m@{ sender, recipient } =
            DH.insertWith (<>) (if sender == loggedUserID then recipient else sender) [ m ] hashMap

      intoContacts userHistory contact@{ user: { id } } = contact
            { history = SU.fromJust $ DH.lookup id userHistory
            }

listSingleContact ∷ Int → Int → ServerEffect Contact
listSingleContact loggedUserID userID = do
      contact ← fromFlatContact <$> SID.presentSingleContact loggedUserID userID
      history ← SID.chatHistoryBetween loggedUserID userID 0
      pure $ contact
            { history = history
            }

processMessage ∷ ∀ r. Int → Int → Int → MessageContent → BaseEffect { storageDetails ∷ Ref StorageDetails, pool ∷ Pool | r } (Tuple Int String)
processMessage loggedUserID userID temporaryID content = do
      sanitized ← processMessageContent content
      id ← SID.insertMessage loggedUserID userID temporaryID sanitized
      pure $ Tuple id sanitized

-- | Sanitizes markdown and handle image uploads
processMessageContent ∷ ∀ r. MessageContent → BaseEffect { storageDetails ∷ Ref StorageDetails, pool ∷ Pool | r } String
processMessageContent content = do
      message ← case content of
            Text m → pure m
            Image caption base64 → do
                  path ← SF.saveBase64File $ base64
                  pure $ "![" <> caption <> "](" <> imageBasePath <> "upload/" <> path <> ")"
      pure <<< DS.trim $ sanitize message

processKarma ∷ ∀ r. Int → Int → Turn → BaseEffect { pool ∷ Pool | r } Unit
processKarma loggedUserID userID turn = SID.insertKarma loggedUserID userID $ SW.karmaFrom turn

blockUser ∷ Int → Int → ServerEffect Ok
blockUser loggedUserID userID = do
      SID.insertBlock loggedUserID userID
      pure ok

listMissedEvents ∷ Int → Maybe Int → Maybe Int → ServerEffect MissedEvents
listMissedEvents loggedUserID lastSenderID lastRecipientID = do
      messageIDs ← DM.maybe (pure []) (SID.messageIDsFor loggedUserID) lastSenderID
      history ← DM.maybe (pure []) (SID.chatHistorySince loggedUserID) lastRecipientID
      contacts ← SID.presentSelectedContacts loggedUserID <<< DA.nubEq $ map _.sender history
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure
            { contacts: intoContacts userHistory <$> map fromFlatContact contacts
            , messageIDs
            }

      where
      intoHashMap hashMap m@{ sender } = DH.insertWith (<>) sender [ m ] hashMap

      intoContacts userHistory contact@{ user: { id } } = contact
            { history = SU.fromJust $ DH.lookup id userHistory
            }

resumeChatHistory ∷ Int → Int → Int → ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserID userID skip = SID.chatHistoryBetween loggedUserID userID skip

reportUser ∷ Int → Report → ServerEffect Ok
reportUser loggedUserID report@{ reason, userID } = do
      SID.insertReport loggedUserID report
      SE.sendEmail "contact@melan.chat" ("[REPORT] " <> show reason) $ "select * from reports where reported = " <> show userID <> ";"
      pure ok