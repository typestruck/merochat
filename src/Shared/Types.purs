module Shared.Types where

import Prelude

import Control.Monad.Except (Except)
import Control.Monad.Except as CME
import Data.Argonaut.Core as DAC
import Data.Argonaut.Core as DAP
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.DateTime (Date, DateTime)
import Data.DateTime as DTT
import Data.DateTime.Instant as DDI
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Data.Time.Duration as DTD
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Flame (Key)
import Foreign (F, Foreign, ForeignError(..))
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Global as G
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.DateTime as SDT
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)
import Web.Event.Internal.Types (Event)

foreign import data Trie :: Type

type NoBody = {}

type BasicUser fields = {
      id :: PrimaryKey,
      name :: String,
      headline :: String,
      description :: String,
      avatar :: Maybe String,
      tags :: Array String,
      karma :: Int,
      karmaPosition :: Int |
      fields
}

type IMUser = (BasicUser (
      gender :: Maybe String,
      country :: Maybe String,
      languages :: Array String,
      age :: Maybe Int
))

type ProfileUser = (BasicUser (
      gender :: Maybe Gender,
      country :: Maybe PrimaryKey,
      languages :: Array PrimaryKey,
      birthday :: Maybe DateWrapper
))

data Gender =
      Female |
      Male |
      NonBinary |
      Other

type PrimaryKey = Int

newtype DateTimeWrapper = DateTimeWrapper DateTime

type EmailCaptcha r = {
      email:: String,
      captchaResponse:: Maybe String |
      r
}

-- | Fields for registration or login
type RegisterLogin = (EmailCaptcha (password :: String))

newtype RegisterLoginUser = RegisterLoginUser {
      id :: PrimaryKey,
      email :: String,
      password :: String
}

type RecoverAccount = EmailCaptcha ()

newtype DateWrapper = DateWrapper Date

type ResetPassword =  {
      token :: String,
      password :: String
}

data Generate =
      Name |
      Headline |
      Description

data By =
      ID PrimaryKey |
      Email String

-- | Errors that should be reported back to the user
data ResponseError =
      BadRequest { reason :: String } |
      InternalError { reason :: String } |
      ExpiredSession

type Suggestion = IMUser

type BasicMessage fields = {
      id :: PrimaryKey |
      fields
}

type ClientMessagePayload = (BasicMessage (
      content :: String,
      userID :: PrimaryKey,
      date :: DateTimeWrapper
))

type Contact = {
      shouldFetchChatHistory :: Boolean, -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
      user :: IMUser,
      chatAge :: Number, --Days,
      chatStarter :: PrimaryKey,
      history :: Array HistoryMessage
}

type HistoryMessage = {
      id :: PrimaryKey,
      sender :: PrimaryKey,
      recipient :: PrimaryKey,
      date :: DateTimeWrapper,
      content :: String,
      status :: MessageStatus
}

data MessageStatus =
      Errored |
      Unread |
      Read

newtype IMUserWrapper = IMUserWrapper IMUser

newtype ContactWrapper = ContactWrapper Contact

newtype HistoryMessageWrapper = HistoryMessageWrapper HistoryMessage

type IM = (
      suggestions :: Array Suggestion,
      contacts :: Array Contact,
      --in case a message from someone blocked was already midway
      blockedUsers :: Array PrimaryKey,
      temporaryID :: PrimaryKey,
      freeToFetchChatHistory :: Boolean,
      freeToFetchContactList :: Boolean,
      message :: Maybe String,
      selectedImage :: Maybe String,
      imageCaption :: Maybe String,
      messageEnter :: Boolean,
      link :: Maybe String,
      suggestionsPage :: Int,
      linkText :: Maybe String,
      isOnline :: Boolean,
      shouldSendMessage :: Boolean,
      --the current logged in user
      user :: IMUser,
      --indexes
      suggesting :: Maybe Int,
      chatting :: Maybe Int,
      --visibility switches
      fullContactProfileVisible :: Boolean,
      userContextMenuVisible :: Boolean,
      displayKarmaLeaderboard :: Boolean,
      profileSettingsToggle :: ProfileSettingsToggle,
      isPreviewing :: Boolean,
      emojisVisible :: Boolean,
      linkFormVisible :: Boolean
)

type IMModel = Record IM

data ProfileSettingsToggle =
      Hidden |
      ShowProfile |
      ShowSettings

type Stats = {
    characters :: Number,
    interest :: Number
}

type Turn = {
    senderStats :: Stats,
    recipientStats:: Stats,
    chatAge :: Number, -- Days,
    replyDelay :: Number --Seconds
}

data MessageContent =
      Image (Tuple String String) |
      Text String

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
      DisplayHistory (Array HistoryMessage)  |
      --user menu
      ConfirmLogout |
      ShowUserContextMenu Event |
      Logout Boolean |
      ToggleKarmaLeaderBoard |
      ToggleProfileSettings ProfileSettingsToggle |
      SetUserContentMenuVisible Boolean |
      SetModalContents (Maybe String) String String |
      --contact
      MarkAsRead |
      ResumeChat PrimaryKey |
      UpdateReadCount |
      CheckFetchContacts |
      FetchContacts Boolean |
      DisplayContacts (Array Contact) |
      DisplayMissedMessages (Array Contact) |
      --suggestion
      PreviousSuggestion |
      NextSuggestion |
      DisplayMoreSuggestions (Array Suggestion) |
      BlockUser PrimaryKey |
      --chat
      ToggleContactProfile |
      DropFile Event |
      EnterBeforeSendMessage Event |
      ForceBeforeSendMessage |
      BeforeSendMessage String |
      SendMessage DateTimeWrapper |
      ReceiveMessage WebSocketPayloadClient Boolean |
      SetMessageContent (Maybe Int) String |
      SelectImage |
      ToggleImageForm (Maybe String) |
      ToggleLinkForm |
      Apply Markup |
      Preview |
      ExitPreview |
      ToggleMessageEnter |
      ToggleEmojisVisible |
      SetEmoji Event |
      InsertLink |
      --main
      PreventStop Event |
      SetNameFromProfile String |
      ToggleOnline |
      CheckMissedMessages |
      SetField (IMModel -> IMModel)

data WebSocketPayloadServer =
      Connect |
      ServerMessage (BasicMessage (
          content :: MessageContent,
          userID :: PrimaryKey,
          turn :: Maybe Turn
      )) |
      ReadMessages {
          --alternatively, update by user?
          ids :: Array PrimaryKey
      } |
      ToBlock {
          id :: PrimaryKey
      }

data WebSocketPayloadClient =
      ClientMessage ClientMessagePayload |
      Received {
          previousID :: PrimaryKey,
          id :: PrimaryKey,
          userID :: PrimaryKey
      } |
      BeenBlocked { id :: PrimaryKey } |
      PayloadError WebSocketPayloadServer

type PM = (
      user :: ProfileUser,
      isCountryVisible :: Boolean,
      isGenderVisible :: Boolean,
      isLanguagesVisible :: Boolean,
      isAgeVisible :: Boolean,
      isTagsVisible :: Boolean,
      countries :: Array (Tuple PrimaryKey String),
      languages :: Array (Tuple PrimaryKey String),
      birthday :: Tuple (Maybe Int) (Tuple (Maybe Int) (Maybe Int))
)

--used to generically set records
type ProfileModel = Record PM

newtype ProfileUserWrapper = ProfileUserWrapper ProfileUser

data ProfileMessage =
      SetPField (ProfileModel -> ProfileModel) |
      SelectAvatar |
      SetAvatar String |
      SetName String |
      SetHeadline String |
      SetDescription String |
      SetTagEnter (Tuple Key String) |
      SetGender String |
      SetCountry String |
      SetYear String |
      SetMonth String |
      SetDay String |
      AddLanguage String |
      RemoveLanguage PrimaryKey Event |
      RemoveTag String Event |
      SaveProfile

type SettingsModel = {
      email :: String,
      emailConfirmation :: String,
      password :: String,
      passwordConfirmation :: String
}

data SettingsMessage =
      SetEmail String |
      SetEmailConfirmation String |
      SetPassword String |
      SetPasswordConfirmation String |
      ChangeEmail |
      ChangePassword |
      TerminateAccount --very bad

derive instance genericMessageStatus :: Generic MessageStatus _
derive instance genericGenerate :: Generic Generate _
derive instance genericGender :: Generic Gender _
derive instance genericResponseError :: Generic ResponseError _
derive instance genericUser :: Generic RegisterLoginUser _
derive instance genericMDateTime :: Generic DateTimeWrapper _
derive instance genericMDate :: Generic DateWrapper _
derive instance genericMessageContent :: Generic MessageContent _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericProfileSettingsToggle :: Generic ProfileSettingsToggle _

derive instance newtypeProfileUserWrapper :: Newtype ProfileUserWrapper _
derive instance newtypeMDateTime :: Newtype DateTimeWrapper _
derive instance newtypeMDate :: Newtype DateWrapper _
derive instance newTypeIMUserWrapper :: Newtype IMUserWrapper _
derive instance newTypeContactWrapper :: Newtype ContactWrapper _
derive instance newTypeHistoryMessageWrapper :: Newtype HistoryMessageWrapper _

derive instance eqGenerate :: Eq Generate
derive instance eqMDateTime :: Eq DateTimeWrapper
derive instance eqMDate :: Eq DateWrapper
derive instance eqGender :: Eq Gender
derive instance eqMessageStatus :: Eq MessageStatus
derive instance eqProfileSettingsToggle :: Eq ProfileSettingsToggle

instance fromSQLRowProfileUserWrapper :: FromSQLRow ProfileUserWrapper where
      fromSQLRow [
            foreignID,
            foreignAvatar,
            foreignGender,
            foreignBirthday,
            foreignUnread,
            foreignHeadline,
            foreignDescription,
            foreignCountry,
            foreignLanguages,
            foreignTags,
            foreignKarma,
            foreignKarmaPosition
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            id <- F.readInt foreignID
            --REFACTOR: all image paths
            avatar <- readAvatar foreignAvatar
            name <- F.readString foreignUnread
            maybeForeignBirthday <- F.readNull foreignBirthday
            birthday <- DM.maybe (pure Nothing) (map (Just <<< DTT.date) <<< SDT.readDate) maybeForeignBirthday
            maybeGender <- F.readNull foreignGender
            gender <- DM.maybe (pure Nothing) (map DSR.read <<< F.readString) maybeGender
            headline <- F.readString foreignHeadline
            description <- F.readString foreignDescription
            maybeCountry <- F.readNull foreignCountry
            country <- DM.maybe (pure Nothing) (map Just <<< F.readInt) maybeCountry
            maybeLanguages :: Maybe Foreign <- F.readNull foreignLanguages
            foreignIDLanguages <- DM.maybe (pure []) F.readArray maybeLanguages
            languages <- DT.traverse F.readInt  foreignIDLanguages
            karma <- F.readInt foreignKarma
            tags <- readTags foreignTags
            karmaPosition <- F.readInt foreignKarmaPosition
            pure $ ProfileUserWrapper {
                  id,
                  avatar,
                  name,
                  birthday: DateWrapper <$> birthday,
                  gender,
                  headline,
                  description,
                  country,
                  karma,
                  languages,
                  tags,
                  karmaPosition
            }
      fromSQLRow _ = Left "missing or extra fields from users table"

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance fromSQLRowResiterLoginUser :: FromSQLRow RegisterLoginUser where
      fromSQLRow [foreignID, foreignEmail, foreignPassword] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            id <- F.readInt foreignID
            email <- F.readString foreignEmail
            password <- F.readString foreignPassword
            pure $ RegisterLoginUser { id, email, password }
      fromSQLRow _ = Left "missing/extra fields from users table"

instance fromSQLRowIMUserWrapper :: FromSQLRow IMUserWrapper where
      fromSQLRow = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept <<< parseIMUserWrapper

instance fromSQLRowContact :: FromSQLRow ContactWrapper where
      fromSQLRow [
            _,
            foreignSender,
            chatAge,
            foreignID,
            foreignAvatar,
            foreignGender,
            foreignAge,
            foreignName,
            foreignHeadline,
            foreignDescription,
            foreignCountry,
            foreignLanguages,
            foreignTags,
            foreignKarma,
            foreignKarmaPosition
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            sender <- F.readInt foreignSender
            chatAge <- F.readNumber chatAge
            IMUserWrapper user <- parseIMUserWrapper [
                  foreignID,
                  foreignAvatar,
                  foreignGender,
                  foreignAge,
                  foreignName,
                  foreignHeadline,
                  foreignDescription,
                  foreignCountry,
                  foreignLanguages,
                  foreignTags,
                  foreignKarma,
                  foreignKarmaPosition
            ]
            pure $ ContactWrapper {
                  shouldFetchChatHistory: true,
                  history: [],
                  chatStarter: sender,
                  chatAge,
                  user
            }
      fromSQLRow _ = Left "missing or extra fields from users table contact projection"

parseIMUserWrapper :: Array Foreign -> F IMUserWrapper
parseIMUserWrapper =
      case _ of
      [     foreignID,
            foreignAvatar,
            foreignGender,
            foreignAge,
            foreignName,
            foreignHeadline,
            foreignDescription,
            foreignCountry,
            foreignLanguages,
            foreignTags,
            foreignKarma,
            foreignKarmaPosition
      ] -> do
            id <- F.readInt foreignID
            avatar <- readAvatar foreignAvatar
            name <- F.readString foreignName
            age <- readAge foreignAge
            maybeGender <- F.readNull foreignGender
            gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
            headline <- F.readString foreignHeadline
            description <- F.readString foreignDescription
            maybeCountry <- F.readNull foreignCountry
            karma <- F.readInt foreignKarma
            karmaPosition <- F.readInt foreignKarmaPosition
            country <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeCountry
            maybeLanguages <- F.readNull foreignLanguages
            languages <- DM.maybe (pure []) (map (DS.split (Pattern ",")) <<< F.readString) maybeLanguages
            tags <- readTags foreignTags
            pure $ IMUserWrapper {
                  id,
                  avatar,
                  name,
                  age,
                  gender,
                  headline,
                  description,
                  karma,
                  country,
                  languages,
                  tags,
                  karmaPosition
            }
      _ ->  CME.throwError <<< DLN.singleton $ ForeignError "missing or extra fields from users table imuser projection"


readAge :: Foreign -> F (Maybe Int)
readAge foreignAge = do
      maybeForeignAge <- F.readNull foreignAge
      DM.maybe (pure Nothing) (map Just <<< F.readInt) maybeForeignAge

readAvatar :: Foreign -> F (Maybe String)
readAvatar foreignAvatar = do
      maybeForeignAvatar <- F.readNull foreignAvatar
      DM.maybe (pure Nothing) (map (Just <<< ("/client/media/upload/" <> _ )) <<< F.readString) maybeForeignAvatar

--REFACTOR: just use pg arrays for tags and languages
readTags foreignTags = do
      maybeTags <- F.readNull foreignTags
      DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags

instance messageWrapperRowFromSQLRow :: FromSQLRow HistoryMessageWrapper where
      fromSQLRow [
          foreignID,
          foreignSender,
          foreignRecipient,
          foreignDate,
          foreignContent,
          foreignStatus
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
          id <- F.readInt foreignID
          sender <- F.readInt foreignSender
          recipient <- F.readInt foreignRecipient
          date <- DateTimeWrapper <$> SDT.readDate foreignDate
          content <- F.readString foreignContent
          status <- SU.fromJust <<< DE.toEnum <$> F.readInt foreignStatus
          pure $ HistoryMessageWrapper { id, sender, recipient, date, content, status }
      fromSQLRow _ = Left "missing or extra fields from users table"

--there is nothing simple about using purescript-simple-json with types other than record
instance writeForeignMDateTime :: WriteForeign DateTimeWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateTimeToNumber
instance writeForeignMessageStatus :: WriteForeign MessageStatus where
      writeImpl messageStatus = F.unsafeToForeign $ DE.fromEnum messageStatus
instance writeForeignGender :: WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender
instance writeForeignMDate :: WriteForeign DateWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateToNumber

instance readForeignMDatee :: ReadForeign DateWrapper where
      readImpl foreignDate = DateWrapper <<< DTT.date <<<  DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDate
instance readForeignMDateTime :: ReadForeign DateTimeWrapper where
      readImpl foreignDateTime = DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDateTime
instance readForeignGender :: ReadForeign Gender where
      readImpl foreignGender = SU.fromJust <<< DSR.read <$> F.readString foreignGender
instance readForeignMessageStatus :: ReadForeign MessageStatus where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance decodeQueryGenerate :: DecodeQueryParam Generate where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) Right $ DSR.read value
                  _ -> errorDecoding query key
instance decodeQueryMDateTime :: DecodeQueryParam DateTimeWrapper where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) (Right <<< DateTimeWrapper <<< DDI.toDateTime) <<< DDI.instant <<< DTD.Milliseconds $ G.readFloat value
                  _ -> errorDecoding query key

errorDecoding :: forall a. Object (Array String) -> String -> Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError {
      values: [],
      message: "Could not decode parameter " <> key,
      key,
      queryObj
}

instance encodeQueryParamMDateTime :: EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< show <<< SDT.dateTimeToNumber
instance encodeQueryGenerate :: EncodeQueryParam Generate where
      encodeQueryParam = Just <<< show

instance showGenerate :: Show Generate where
      show = DGRS.genericShow
instance showMessageStatus :: Show MessageStatus where
      show = DGRS.genericShow
instance showResponseError :: Show ResponseError where
      show = DGRS.genericShow
instance showGender :: Show Gender where
      show Female = "Female"
      show Male = "Male"
      show NonBinary = "Non binary"
      show Other = "Other"
instance showMDateTime :: Show DateTimeWrapper where
      show = DGRS.genericShow
instance showMDate :: Show DateWrapper where
      show = DGRS.genericShow
instance showMessageContent :: Show MessageContent where
      show = DGRS.genericShow
instance showWebSocketPayloadClient :: Show WebSocketPayloadClient where
      show = DGRS.genericShow
instance showWebSocketPayloadServer :: Show WebSocketPayloadServer where
      show = DGRS.genericShow
instance showProfileSettingsToggle :: Show ProfileSettingsToggle where
      show = DGRS.genericShow

instance toSQLValueGender :: ToSQLValue Gender where
      toSQLValue = F.unsafeToForeign <<< show

instance fromSQLValueGender :: FromSQLValue Gender where
      fromSQLValue = DB.lmap show <<< CME.runExcept <<< map (SU.fromJust <<< DSR.read) <<< F.readString

instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonGender :: EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMDateTime :: EncodeJson DateTimeWrapper where
      encodeJson = DAC.fromNumber <<< SDT.dateTimeToNumber
instance encodeJsonMDate :: EncodeJson DateWrapper where
      encodeJson = DAC.fromNumber <<< SDT.dateToNumber
instance encodeJsonWebSocketPayloadServer :: EncodeJson WebSocketPayloadServer where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMessageContent :: EncodeJson MessageContent where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonProfileSettingsToggle :: EncodeJson ProfileSettingsToggle where
      encodeJson = DAEGR.genericEncodeJson

instance decodeJsonMessageStatus :: DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonGender :: DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonMDateTime :: DecodeJson DateTimeWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldnt parse epoch") (Right <<< DateTimeWrapper <<< DDI.toDateTime) <<< DAP.caseJsonNumber (Nothing) (DDI.instant <<< DTD.Milliseconds)
instance decodeJsonMDate :: DecodeJson DateWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldnt parse epoch") (Right <<< DateWrapper <<< DTT.date <<< DDI.toDateTime) <<< DAP.caseJsonNumber (Nothing) (DDI.instant <<< DTD.Milliseconds)
instance decodeJsonWebSocketPayloadServer :: DecodeJson WebSocketPayloadServer where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonMessageContent :: DecodeJson MessageContent where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonProfileSettingsToggle :: DecodeJson ProfileSettingsToggle where
      decodeJson = DADGR.genericDecodeJson

instance readGender :: Read Gender where
      read input =
          case DS.toLower $ DS.trim input of
              "female" -> Just Female
              "male" -> Just Male
              "non binary" -> Just NonBinary
              "other" -> Just Other
              _ -> Nothing

instance readGenerate :: Read Generate where
      read input =
          case DS.toLower $ DS.trim input of
              "name" -> Just Name
              "headline" -> Just Headline
              "description" -> Just Description
              _ -> Nothing

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

      fromEnum = case _ of
          Errored -> -1
          Unread -> 0
          Read -> 1

      toEnum = case _ of
          -1 -> Just Errored
          0 -> Just Unread
          1 -> Just Read
          _ -> Nothing

instance enumMessageStatus :: Enum MessageStatus where
      succ = case _ of
          Errored -> Just Unread
          Unread -> Just Read
          Read -> Nothing

      pred = case _ of
          Errored -> Nothing
          Unread -> Just Errored
          Read -> Just Unread