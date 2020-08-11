module Shared.IM.Types where

import Prelude

import Control.Monad.Except (Except)
import Control.Monad.Except as CME
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Array as DA
import Data.Bifunctor as DB
import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (DateTime(..))
import Data.DateTime (DateTime, Time(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.Hashable (class Hashable)
import Data.Hashable as DH
import Data.Int as DIN
import Data.Int53 (Int53)
import Data.JSDate (JSDate)
import Data.JSDate as DJ
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.Time.Duration (Days(..), Seconds(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Database.PostgreSQL as DP
import Effect.Now as DN
import Effect.Now as EN
import Effect.Unsafe as EU
import Foreign (Foreign, ForeignError(..))
import Foreign as F
import Partial.Unsafe as PU
import Shared.DateTime as SDT
import Shared.Types (MDate(..), MDateTime(..), PrimaryKey(..), parsePrimaryKey, parseInt)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.Socket.WebSocket (WebSocket)

type Suggestion = IMUser

type BasicUser fields = {
      id :: PrimaryKey,
      name :: String,
      headline :: String,
      description :: String |
      fields
}

type BasicMessage fields = {
      id :: PrimaryKey |
      fields
}

type ClientMessagePayload = (BasicMessage (
      content :: String,
      userID :: PrimaryKey,
      date :: MDateTime
))

--fields needed by the IM page
newtype IMUser = IMUser (BasicUser (
      avatar :: Maybe String,
      gender :: Maybe String,
      country :: Maybe String,
      languages :: Array String,
      tags :: Array String,
      age :: Maybe Int,
      karma :: Int
))

newtype Contact = Contact {
      shouldFetchChatHistory :: Boolean, -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
      user :: IMUser,
      chatAge :: Number, --Days,
      chatStarter :: PrimaryKey,
      history :: Array HistoryMessage
}

newtype IMModel = IMModel {
      suggestions :: Array Suggestion,
      contacts :: Array Contact,
      temporaryID :: PrimaryKey,
      freeToFetchChatHistory :: Boolean,
      freeToFetchContactList :: Boolean,
      message :: Maybe String,
      selectedImage :: Maybe String,
      imageCaption :: Maybe String,
      messageEnter :: Boolean,
      --the current logged in user
      user :: IMUser,
      --indexes
      suggesting :: Maybe Int,
      chatting :: Maybe Int,
      --visibility switches
      userContextMenuVisible :: Boolean,
      profileSettingsToggle :: ProfileSettingsToggle,
      isPreviewing :: Boolean,
      emojisVisible :: Boolean
}

newtype HistoryMessage = HistoryMessage {
      id :: PrimaryKey,
      sender :: PrimaryKey,
      recipient :: PrimaryKey,
      date :: MDateTime,
      content :: String,
      status :: MessageStatus
}

newtype Stats = Stats {
    characters :: Number,
    interest :: Number
}

newtype Turn = Turn {
    senderStats :: Stats,
    recipientStats:: Stats,
    chatAge :: Number, -- Days,
    replyDelay :: Number --Seconds
}

newtype SuggestionsPayload = SuggestionsPayload (Array IMUser)
newtype HistoryPayload = HistoryPayload (Array HistoryMessage)
newtype SingleContactPayload = SingleContactPayload (Array Contact)
newtype ContactsPayload = ContactsPayload (Array Contact)
newtype ProfileSettingsPayload = ProfileSettingsPayload String

data MessageContent =
      Image (Tuple String String) |
      Text String

data ProfileSettingsToggle =
      Hidden |
      ShowProfile |
      ShowSettings

data MessageStatus =
      Unread |
      Read

data Markup =
      Bold |
      Italic |
      Strike |
      Heading |
      OrderedList |
      UnorderedList

data IMMessage =
      --history
      CheckFetchHistory |
      FetchHistory Boolean |
      DisplayHistory HistoryPayload  |
      --user menu
      ConfirmLogout |
      ShowUserContextMenu Event |
      Logout Boolean |
      ToggleProfileSettings ProfileSettingsToggle |
      SetUserContentMenuVisible Boolean |
      SetModalContents (Maybe String) String ProfileSettingsPayload |
      --contact
      MarkAsRead |
      ResumeChat PrimaryKey |
      UpdateReadCount |
      CheckFetchContacts |
      FetchContacts Boolean |
      DisplayContacts ContactsPayload  |
      --suggestion
      PreviousSuggestion |
      NextSuggestion |
      DisplayMoreSuggestions SuggestionsPayload |
      --chat
      DropFile Event |
      SetUpMessage Event |
      BeforeSendMessage Boolean String |
      SendMessage MDateTime |
      ReceiveMessage WebSocketPayloadClient Boolean |
      SetMessageContent (Maybe Int) String |
      SelectImage |
      ToggleImageForm (Maybe String) |
      Apply Markup |
      Preview |
      ExitPreview |
      SetImageCaption String |
      ToggleMessageEnter |
      ToggleEmojisVisible |
      SetEmoji Event |
      --main
      PreventStop Event |
      SetName String

data WebSocketPayloadServer =
      Connect String |
      ServerMessage (BasicMessage (
            content :: MessageContent,
            token :: String,
            userID :: PrimaryKey,
            turn :: Maybe Turn
      )) |
      ReadMessages {
            token :: String,
            --alternatively, update by user?
            ids :: Array PrimaryKey
      }

data WebSocketPayloadClient =
      ClientMessage ClientMessagePayload |
      Received {
            previousID :: PrimaryKey,
            id :: PrimaryKey
      }

derive instance genericMessageContent :: Generic MessageContent _
derive instance genericProfileSettingsPayload :: Generic ProfileSettingsPayload _
derive instance genericContactsPayload :: Generic ContactsPayload _
derive instance genericSingleContactPayload :: Generic SingleContactPayload _
derive instance genericHistoryPayload :: Generic HistoryPayload _
derive instance genericSuggestionsPayload :: Generic SuggestionsPayload _
derive instance genericStats :: Generic Stats _
derive instance genericTurn :: Generic Turn _
derive instance genericContact :: Generic Contact _
derive instance genericIMUser :: Generic IMUser _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericIMModel :: Generic IMModel _
derive instance genericHistoryMessage :: Generic HistoryMessage _
derive instance genericMessageStatus :: Generic MessageStatus _
derive instance genericProfileSettingsToggle :: Generic ProfileSettingsToggle _

derive instance newTypeIMUser :: Newtype IMUser _
derive instance newTypeContact :: Newtype Contact _
derive instance newTypeHistoryMessage :: Newtype HistoryMessage _
derive instance newTypeIMModel :: Newtype IMModel _

derive instance eqHistoryMessage :: Eq HistoryMessage
derive instance eqIMModel :: Eq IMModel
derive instance eqContact :: Eq Contact
derive instance eqIMUser :: Eq IMUser
derive instance eqStats :: Eq Stats
derive instance eqTurn :: Eq Turn
derive instance eqMessageStatus :: Eq MessageStatus
derive instance eqProfileSettingsToggle :: Eq ProfileSettingsToggle

instance showMessageContent :: Show MessageContent where
      show = DGRS.genericShow
instance showStats :: Show Stats where
      show = DGRS.genericShow
instance showTurn :: Show Turn where
      show = DGRS.genericShow
instance showContact :: Show Contact where
      show = DGRS.genericShow
instance showHistoryMessage :: Show HistoryMessage where
      show = DGRS.genericShow
instance showIMUser :: Show IMUser where
      show = DGRS.genericShow
instance showWebSocketPayloadClient :: Show WebSocketPayloadClient where
      show = DGRS.genericShow
instance showWebSocketPayloadServer :: Show WebSocketPayloadServer where
      show = DGRS.genericShow
instance showIMModel :: Show IMModel where
      show = DGRS.genericShow
instance showMessageStatus :: Show MessageStatus where
      show = DGRS.genericShow
instance showProfileSettingsToggle :: Show ProfileSettingsToggle where
      show = DGRS.genericShow

instance encodeJsonMessageContent :: EncodeJson MessageContent where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonContact :: EncodeJson Contact where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonProfileSettingsToggle :: EncodeJson ProfileSettingsToggle where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonIMUser :: EncodeJson IMUser where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonHistoryMessage :: EncodeJson HistoryMessage where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonTurn :: EncodeJson Turn where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonStats :: EncodeJson Stats where
      encodeJson = DAEGR.genericEncodeJson

instance decodeJsonMessageContent :: DecodeJson MessageContent where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonContact :: DecodeJson Contact where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonProfileSettingsToggle :: DecodeJson ProfileSettingsToggle where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonHistoryMessage :: DecodeJson HistoryMessage where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonMessageStatus :: DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonIMUser :: DecodeJson IMUser where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonTurn :: DecodeJson Turn where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonStats :: DecodeJson Stats where
      decodeJson = DADGR.genericDecodeJson

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance fromSQLRowIMUser :: FromSQLRow IMUser where
      fromSQLRow= DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept <<< parseIMUser

instance fromSQLRowContact :: FromSQLRow Contact where
      fromSQLRow [
            _,
            foreignSender,
            foreignFirstMessageDate,
            foreignID,
            foreignAvatar,
            foreignGender,
            foreignBirthday,
            foreignName,
            foreignHeadline,
            foreignDescription,
            foreignCountry,
            foreignLanguages,
            foreignTags,
            foreignKarma
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            sender <- parsePrimaryKey foreignSender
            firstMessageDate <- SU.fromJust <<< DJ.toDate <$> DJ.readDate foreignFirstMessageDate
            user <- parseIMUser [
                  foreignID,
                  foreignAvatar,
                  foreignGender,
                  foreignBirthday,
                  foreignName,
                  foreignHeadline,
                  foreignDescription,
                  foreignCountry,
                  foreignLanguages,
                  foreignTags,
                  foreignKarma
            ]
            pure $ Contact {
                  shouldFetchChatHistory: true,
                  history: [],
                  chatAge: DN.unwrap (DD.diff (EU.unsafePerformEffect DN.nowDate) firstMessageDate :: Days),
                  chatStarter: sender,
                  user
            }
      fromSQLRow _ = Left "missing or extra fields from users table contact projection"

parseIMUser :: Array Foreign -> Except (NonEmptyList ForeignError) IMUser
parseIMUser [
      foreignID,
      foreignAvatar,
      foreignGender,
      foreignBirthday,
      foreignName,
      foreignHeadline,
      foreignDescription,
      foreignCountry,
      foreignLanguages,
      foreignTags,
      foreignKarma
] = do
      id <- parsePrimaryKey foreignID
      maybeForeignerAvatar <- F.readNull foreignAvatar
      avatar <- DM.maybe (pure Nothing) (map (Just <<< ("/client/media/upload/" <> _ )) <<< F.readString) maybeForeignerAvatar
      name <- F.readString foreignName
      maybeForeignerBirthday <- F.readNull foreignBirthday
      birthday <- DM.maybe (pure Nothing) (map DJ.toDate <<< DJ.readDate) maybeForeignerBirthday
      maybeGender <- F.readNull foreignGender
      gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
      headline <- F.readString foreignHeadline
      description <- F.readString foreignDescription
      maybeCountry <- F.readNull foreignCountry
      karma <- parseInt foreignKarma
      country <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeCountry
      maybeLanguages <- F.readNull foreignLanguages
      languages <- DM.maybe (pure []) (map (DS.split (Pattern ",")) <<< F.readString) maybeLanguages
      maybeTags <- F.readNull foreignTags
      tags <- DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags
      pure $ IMUser {
            id,
            avatar,
            name,
            age: SDT.ageFrom birthday,
            gender,
            headline,
            description,
            karma,
            country,
            languages,
            tags
      }
parseIMUser _ =  CME.throwError <<< DLN.singleton $ ForeignError "missing or extra fields from users table imuser projection"

instance messageRowFromSQLRow :: FromSQLRow HistoryMessage where
      fromSQLRow [
            foreignID,
            foreignSender,
            foreignRecipient,
            foreignDate,
            foreignContent,
            foreignStatus
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            id <- parsePrimaryKey foreignID
            sender <- parsePrimaryKey foreignSender
            recipient <- parsePrimaryKey foreignRecipient
            date <- MDateTime <<< SU.fromJust <<< DJ.toDateTime <$> DJ.readDate foreignDate
            content <- F.readString foreignContent
            status <- SU.fromJust <<< DE.toEnum <$> F.readInt foreignStatus
            pure $ HistoryMessage { id, sender, recipient, date, content, status }
      fromSQLRow _ = Left "missing or extra fields from users table"

--thats a lot of work...
instance ordMessageStatus :: Ord MessageStatus where
      compare Unread Read = LT
      compare Read Unread = GT
      compare _ _ = EQ

instance boundedMessageStatus :: Bounded MessageStatus where
      bottom = Unread
      top = Read

instance boundedEnumMessageStatus :: BoundedEnum MessageStatus where
      cardinality = Cardinality 1

      fromEnum Unread = 0
      fromEnum Read = 1

      toEnum 0 = Just Unread
      toEnum 1 = Just Read
      toEnum _ = Nothing

instance enumMessageStatus :: Enum MessageStatus where
      succ Unread = Just Read
      succ Read = Nothing

      pred Unread = Nothing
      pred Read = Just Unread