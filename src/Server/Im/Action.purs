module Server.Im.Action where

import Debug
import Prelude
import Shared.Im.Types
import Shared.Privilege

import Data.Array ((:))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable as DN
import Data.Set (Set)
import Data.Set as DST
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Environment (production)
import Run.Except as RE
import Safe.Coerce as SC
import Server.AccountValidation as SA
import Server.Database.Types (Checked(..))
import Server.Effect (BaseEffect, ServerEffect)
import Server.Email (Email(..))
import Server.Email as SE
import Server.File as SF
import Server.Im.Database.Changelog as SIDC
import Server.Im.Database.Execute as SIDE
import Server.Im.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.Im.Database.Flat as SIF
import Server.Im.Database.Permission as SIDPP
import Server.Im.Database.Present as SIDP
import Server.Im.Database.Suggest as SIDS
import Server.Im.Types (Payload)
import Shared.Content
import Server.Sanitize as SS
import Server.ThreeK as ST
import Server.Wheel as SW
import Shared.Backer.Contact (backerUser)
import Shared.Backer.Contact as SBC
import Shared.Changelog (Changelog)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SD
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))

im ∷ Int → ServerEffect Payload
im loggedUserId = do
      maybeUser ← SIDP.presentUser loggedUserId
      case maybeUser of
            --happens if the user has an invalid cookie/was suspended
            Nothing → RE.throw ExpiredSession
            Just user → do
                  suggestions ← suggest loggedUserId 0 ThisWeek
                  contacts ← listContacts loggedUserId 0
                  let shouldDonate = not (SC.coerce user.backer) && SD.daysDiff user.joined > 3
                  pure
                        { contacts: if shouldDonate then SBC.backerContact user.id : contacts else contacts
                        , suggestions: if shouldDonate then DA.snoc suggestions backerUser else suggestions
                        , user: SIF.fromFlatUser user
                        }

suggest ∷ Int → Int → SuggestionsFrom → ServerEffect (Array Suggestion)
suggest loggedUserId skip sg = map SIF.fromFlatUser <$> SIDS.suggest loggedUserId skip sg

listContacts ∷ Int → Int → ServerEffect (Array Contact)
listContacts loggedUserId skip = presentContacts <$> SIDP.presentContacts loggedUserId skip

listSingleContact ∷ Int → Int → ServerEffect (Array Contact)
listSingleContact loggedUserId userId = presentContacts <$> SIDP.presentSingleContact loggedUserId userId 0

resumeChatHistory ∷ Int → Int → Int → ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserId userId skip = map fromFlatMessage <$> SIDP.presentSingleContact loggedUserId userId skip

listMissedContacts ∷ Int → DateTimeWrapper → Maybe Int → ServerEffect (Array Contact)
listMissedContacts loggedUserId (DateTimeWrapper dt) lastSentId = presentContacts <$> SIDP.presentMissedContacts loggedUserId dt lastSentId

presentContacts ∷ Array FlatContactHistoryMessage → Array Contact
presentContacts = map chatHistory <<< DA.groupBy sameContact
      where
      sameContact a b = a.id == b.id

      chatHistory records = (fromFlatContact $ DAN.head records) { history = fromFlatMessage <$> DAN.toArray records }

subscribe ∷ Int → ServerEffect Unit
subscribe loggedUserId = SIDE.subscribe loggedUserId

processMessage ∷ ∀ r. Int → Int → Content → BaseEffect { pool ∷ Pool | r } (Either MessageError (Int /\ String))
processMessage loggedUserId userId content = do
      isIt ← SIDE.isRecipientVisible loggedUserId userId
      if isIt then do
            privileges ← markdownPrivileges loggedUserId
            sanitized ← processMessageContent content privileges
            if DS.null sanitized then
                  pure $ Left InvalidMessage
            else do
                  id ← SIDE.insertMessage loggedUserId userId sanitized
                  pure $ Right (id /\ sanitized)
      else
            pure $ Left UserUnavailable

listChangelogs :: Int -> Maybe Int -> ServerEffect (Array Changelog)
listChangelogs loggedUserId id = SIDC.listChangelogs loggedUserId id

markRead :: Int -> Array Int -> ServerEffect Unit
markRead loggedUserId ids = SIDC.markRead loggedUserId ids

editMessage ∷ ∀ r. Int → Int → Int → Content → BaseEffect {  pool ∷ Pool | r } (Either MessageError String)
editMessage loggedUserId userId messageId content = do
      isVisible ← SIDE.isRecipientVisible loggedUserId userId
      canEdit ← if isVisible then SIDPP.canEditMessage loggedUserId messageId else pure false
      if canEdit then do
            privileges ← markdownPrivileges loggedUserId
            sanitized ← processMessageContent content privileges
            if DS.null sanitized then
                  pure $ Left InvalidMessage
            else do
                  SIDE.updateMessage messageId sanitized
                  pure $ Right sanitized
      else
            pure $ Left UserUnavailable

unsendMessage ∷ ∀ r. Int → Int → Int → BaseEffect {  pool ∷ Pool | r } Unit
unsendMessage loggedUserId userId messageId = SIDE.deleteMessage loggedUserId userId messageId

markdownPrivileges ∷ ∀ r. Int → BaseEffect { pool ∷ Pool | r } (Set Privilege)
markdownPrivileges loggedUserId = (DST.fromFoldable <<< map _.feature) <$> SIDPP.markdownPrivileges loggedUserId

-- | Sanitizes markdown and handle image uploads
processMessageContent ∷ ∀ r. Content → Set Privilege → BaseEffect {  pool ∷ Pool | r } String
processMessageContent content privileges = do
      message ← case content of
            Text m | allowed m → pure m
            Image caption width height base64 | DST.member SendImages privileges → do
                  name ← SF.saveBase64File base64
                  pure $ "![" <> caption <> "]([" <> show width <> "," <> show height <> "]" <> SP.resourcePath (Left $ Upload name) Ignore <> ")"
            Audio base64 | DST.member SendAudios privileges → do
                  name ← SF.saveBase64File base64
                  pure $ "<audio controls src='" <> SP.resourcePath (Left $ Upload name) Ignore <> "'></audio>"
            _ → pure ""
      pure <<< DS.trim $ SS.sanitize message
      where
      allowed contents = DST.member SendImages privileges || noLinks contents
      noLinks contents =
            let
                  canSendLinks = DST.member SendLinks privileges
            in
                  DM.isNothing <<< DA.find (link canSendLinks) $ SM.lexer contents
      link canSendLinks (Token t) = DM.isJust (DN.toMaybe t.tokens >>= DA.find (isLink canSendLinks))
      isLink canSendLinks (Token child) = child."type" == "image" || not canSendLinks && ((child."type" == "link" || child."type" == "reflink") && child.raw /= child.text)

processKarma ∷ ∀ r. Int → Int → Turn → BaseEffect { pool ∷ Pool | r } Unit
processKarma loggedUserId userId turn = SIDE.insertKarma loggedUserId userId $ SW.karmaFrom turn

blockUser ∷ Int → Int → ServerEffect Unit
blockUser loggedUserId userId = SIDE.insertBlock loggedUserId userId

deleteChat ∷ Int → { userId ∷ Int, messageId ∷ Int } → ServerEffect Unit
deleteChat loggedUserId ids@{ userId } = do
      entry ← SIDE.chatHistoryEntry loggedUserId userId
      case entry of
            Nothing → pure unit
            Just { sender } → SIDE.markAsDeleted (sender == loggedUserId) loggedUserId ids

reportUser ∷ Int → Report → ServerEffect Unit
reportUser loggedUserId report = do
      void $ SIDE.insertReport loggedUserId report
      SE.sendEmail $ SE.Report
            { reported: report.userId
            , reporter: loggedUserId
            , reason: show report.reason
            , comment: DM.fromMaybe "" report.comment
            }

finishTutorial ∷ Int → ServerEffect Unit
finishTutorial loggedUserId = SIDE.updateTutorialCompleted loggedUserId

--makeshift action so new users have more attention
greet ∷ Int → ServerEffect Unit
greet loggedUserId =
      if production then do
            starter ← ST.generateConversationStarter
            void $ SIDE.insertMessage sender loggedUserId ( "hey there! " <> starter)
      else
            pure unit
      where
      sender = 4

registerUser ∷ Int → String → String → ServerEffect Unit
registerUser loggedUserId rawEmail password = do
      email ← SA.validateEmail rawEmail
      hash ← SA.validatePassword password
      SA.validateExistingEmail email
      SIDE.registerUser loggedUserId email hash
