module Shared.Types where

import Prelude

import Control.Monad.Except (Except)
import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Date as DD
import Data.DateTime (Date, DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Either as DET
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.Hashable (class Hashable)
import Data.Hashable as DH
import Data.Int as DIN
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.JSDate (JSDate)
import Data.JSDate as DJ
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Data.Time.Duration (Days)
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Debug.Trace (spy)
import Effect.Now as ED
import Effect.Unsafe as EU
import Flame (Key)
import Foreign (F, Foreign, ForeignError(..))
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.DateTime as SDT
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce as UC
import Web.Event.Internal.Types (Event)

foreign import data Trie :: Type

foreign import fromJSDate :: JSDate -> Json
foreign import fromInt53 :: Int53 -> Json
foreign import toInt53 :: Json -> Int53

type NoBody = {}

type BasicUser fields = {
      id :: PrimaryKey,
      name :: String,
      headline :: String,
      description :: String |
      fields
}

type IMUser = (BasicUser (
      avatar :: Maybe String,
      gender :: Maybe String,
      country :: Maybe String,
      languages :: Array String,
      tags :: Array String,
      age :: Maybe Int,
      karma :: Int
))

type ProfileUser = (BasicUser (
      avatar ::  Maybe String,
      gender :: Maybe Gender,
      country :: Maybe PrimaryKey,
      languages :: Array PrimaryKey,
      tags :: Array String,
      birthday :: Maybe DateWrapper,
      karma :: Int
))

data Gender =
      Female |
      Male |
      NonBinary |
      Other

newtype PrimaryKey = PrimaryKey Int53

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
      InternalError { reason :: String }

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
      --the current logged in user
      user :: IMUser,
      --indexes
      suggesting :: Maybe Int,
      chatting :: Maybe Int,
      --visibility switches
      fullContactProfileVisible :: Boolean,
      userContextMenuVisible :: Boolean,
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
      SetUpMessage Event |
      BeforeSendMessage Boolean String |
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
derive instance genericPrimaryKey :: Generic PrimaryKey _
derive instance genericUser :: Generic RegisterLoginUser _
derive instance genericMDateTime :: Generic DateTimeWrapper _
derive instance genericMDate :: Generic DateWrapper _
derive instance genericMessageContent :: Generic MessageContent _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericProfileSettingsToggle :: Generic ProfileSettingsToggle _

derive instance newtypeProfileUserWrapper :: Newtype ProfileUserWrapper _
derive instance newtypeMDateTime :: Newtype DateTimeWrapper _
derive instance newtypePrimaryKey :: Newtype PrimaryKey _
derive instance newtypeMDate :: Newtype DateWrapper _
derive instance newTypeIMUserWrapper :: Newtype IMUserWrapper _
derive instance newTypeContactWrapper :: Newtype ContactWrapper _
derive instance newTypeHistoryMessageWrapper :: Newtype HistoryMessageWrapper _

derive instance eqGenerate :: Eq Generate
derive instance eqMDateTime :: Eq DateTimeWrapper
derive instance eqMDate :: Eq DateWrapper
derive instance eqGender :: Eq Gender
derive instance eqPrimaryKey :: Eq PrimaryKey
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
            foreignTags,--REFACTOR: avoid code duplication here and on im types
            foreignKarma
      ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            id <- parsePrimaryKey foreignID
            maybeForeignerAvatar <- F.readNull foreignAvatar
            --REFACTOR: all image paths
            avatar <- DM.maybe (pure Nothing) (map (Just <<< ("/client/media/upload/" <> _ )) <<< F.readString)  maybeForeignerAvatar
            name <- F.readString foreignUnread
            maybeForeignerBirthday <- F.readNull foreignBirthday
            birthday <- DM.maybe (pure Nothing) (map DJ.toDate <<< DJ.readDate) maybeForeignerBirthday
            maybeGender <- F.readNull foreignGender
            gender <- DM.maybe (pure Nothing) (map DSR.read <<< F.readString) maybeGender
            headline <- F.readString foreignHeadline
            description <- F.readString foreignDescription
            maybeCountry <- F.readNull foreignCountry
            country <- DM.maybe (pure Nothing) (map Just <<< parsePrimaryKey) maybeCountry
            maybeLanguages :: Maybe Foreign <- F.readNull foreignLanguages
            foreignIDLanguages <- DM.maybe (pure []) F.readArray maybeLanguages
            languages <- DT.traverse parsePrimaryKey  foreignIDLanguages
            karma <- parseInt foreignKarma
            maybeTags <- F.readNull foreignTags
            tags <- DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags
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
                  tags
            }
      fromSQLRow _ = Left "missing or extra fields from users table"

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance fromSQLRowResiterLoginUser :: FromSQLRow RegisterLoginUser where
      fromSQLRow [foreignID, foreignEmail, foreignPassword] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
            id <- parsePrimaryKey foreignID
            email <- F.readString foreignEmail
            password <- F.readString foreignPassword
            pure $ RegisterLoginUser { id, email, password }
      fromSQLRow _ = Left "missing/extra fields from users table"

instance fromSQLRowIMUserWrapper :: FromSQLRow IMUserWrapper where
      fromSQLRow= DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept <<< parseIMUserWrapper

instance fromSQLRowContact :: FromSQLRow ContactWrapper where
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
            IMUserWrapper user <- parseIMUserWrapper [
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
            pure $ ContactWrapper {
                  shouldFetchChatHistory: true,
                  history: [],
                  --REFACTOR: just get this age (and user age) from the database (pg has the function age....)
                  chatAge: DN.unwrap (DD.diff (EU.unsafePerformEffect ED.nowDate) firstMessageDate :: Days),
                  chatStarter: sender,
                  user
            }
      fromSQLRow a = let c = spy "a" a in Left "missing or extra fields from users table contact projection"

parseIMUserWrapper :: Array Foreign -> Except (NonEmptyList ForeignError) IMUserWrapper
parseIMUserWrapper =
      case _ of
      [     foreignID,
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
      ] -> do
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
            pure $ IMUserWrapper {
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
      _ ->  CME.throwError <<< DLN.singleton $ ForeignError "missing or extra fields from users table imuser projection"

instance messageWrapperRowFromSQLRow :: FromSQLRow HistoryMessageWrapper where
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
          date <- DateTimeWrapper <<< SU.fromJust <<< DJ.toDateTime <$> DJ.readDate foreignDate
          content <- F.readString foreignContent
          status <- SU.fromJust <<< DE.toEnum <$> F.readInt foreignStatus
          pure $ HistoryMessageWrapper { id, sender, recipient, date, content, status }
      fromSQLRow _ = Left "missing or extra fields from users table"

--there is nothing simple about using purescript-simple-json with types other than record
instance writeForeignPrimaryKey :: WriteForeign PrimaryKey where
      writeImpl (PrimaryKey id) = F.unsafeToForeign $ DI.toString id
instance writeForeignMDateTime :: WriteForeign DateTimeWrapper where
      writeImpl (DateTimeWrapper dateTime) = F.unsafeToForeign $ SDT.formatDateTime dateTime
instance writeForeignMessageStatus :: WriteForeign MessageStatus where
      writeImpl messageStatus = F.unsafeToForeign $ DE.fromEnum messageStatus
instance writeForeignGender :: WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender
instance writeForeignMDate :: WriteForeign DateWrapper where
      writeImpl (DateWrapper date) = F.unsafeToForeign $ SDT.formatDate date

instance readForeignMDatee :: ReadForeign DateWrapper where
      readImpl date = DateWrapper <<< SU.fromRight <<< SDT.unformatDate <$> F.readString date
instance readForeignPrimaryKey :: ReadForeign PrimaryKey where
      readImpl id = PrimaryKey <<< SU.fromJust <<< DI.fromString <$> F.readString id
instance readForeignMDateTime :: ReadForeign DateTimeWrapper where
      readImpl dateTime = DateTimeWrapper <<< SU.fromRight <<< SDT.unformatDateTime <$> F.readString dateTime
instance readForeignGender :: ReadForeign Gender where
      readImpl value = SU.fromJust <<< DSR.read <$> F.readString value
instance readForeignMessageStatus :: ReadForeign MessageStatus where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance decodeQueryParamPrimaryKey :: DecodeQueryParam PrimaryKey where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) (Right <<< PrimaryKey) $ DI.fromString value
                  _ -> errorDecoding query key
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
                  Just [value] -> DET.either (const (errorDecoding query key)) (Right <<< DateTimeWrapper) $ SDT.unformatDateTime value
                  _ -> errorDecoding query key

errorDecoding :: forall a. Object (Array String) -> String -> Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError {
      values: [],
      message: "Could not decode parameter " <> key,
      key,
      queryObj
}

instance encodeQueryParamPrimaryKey :: EncodeQueryParam PrimaryKey where
      encodeQueryParam = Just <<< DI.toString <<< DN.unwrap
instance encodeQueryParamMDateTime :: EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< SDT.formatDateTime <<< DN.unwrap
instance encodeQueryGenerate :: EncodeQueryParam Generate where
      encodeQueryParam = Just <<< show

instance showGenerate :: Show Generate where
      show = DGRS.genericShow
instance showMessageStatus :: Show MessageStatus where
      show = DGRS.genericShow
instance showResponseError :: Show ResponseError where
      show = DGRS.genericShow
instance showPrimaryKey :: Show PrimaryKey where
      show (PrimaryKey i) = DI.toString i
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

instance primaryKeySemiring :: Semiring PrimaryKey where
      add (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a + b)
      zero = PrimaryKey $ DI.fromInt 0
      mul (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a * b)
      one = PrimaryKey $ DI.fromInt 1

instance hashablePrimaryKey :: Hashable PrimaryKey where
      hash (PrimaryKey key) = DH.hash $ DI.toNumber key

instance toSQLValuePrimaryKey :: ToSQLValue PrimaryKey where
      toSQLValue (PrimaryKey int53) = F.unsafeToForeign $ DI.toNumber int53
instance toSQLValueGender :: ToSQLValue Gender where
      toSQLValue = F.unsafeToForeign <<< show

instance fromSQLValuePrimaryKey :: FromSQLValue PrimaryKey where
      fromSQLValue = DB.lmap show <<< CME.runExcept <<< parsePrimaryKey
instance fromSQLValueGender :: FromSQLValue Gender where
      fromSQLValue = DB.lmap show <<< CME.runExcept <<< map (SU.fromJust <<< DSR.read) <<< F.readString

--these functions are needed cos javascript is crap and numbers from postgresql are parsed as strings

parsePrimaryKey :: Foreign -> F PrimaryKey
parsePrimaryKey data_
      | F.typeOf data_ == "number" = map (PrimaryKey <<< SU.fromJust <<< DI.fromNumber) $ F.readNumber data_
      | otherwise = PrimaryKey <<< SU.fromJust <<< DI.fromString <$> F.readString data_

parseInt :: Foreign -> F Int
parseInt data_
      | F.typeOf data_ == "number" = F.readInt data_
      | otherwise = SU.fromJust <<< DIN.fromString <$> F.readString data_

instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonGender :: EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
      encodeJson (PrimaryKey id) = fromInt53 id
instance encodeJsonMDateTime :: EncodeJson DateTimeWrapper where
      encodeJson (DateTimeWrapper dateTime) = fromJSDate $ DJ.fromDateTime dateTime
instance encodeJsonMDate :: EncodeJson DateWrapper where
      encodeJson (DateWrapper date) = fromJSDate <<< DJ.fromDateTime <<< DateTime date $ Time (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0)
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
instance decodeJsonPrimaryKey :: DecodeJson PrimaryKey where
      decodeJson = Right <<< PrimaryKey <<< toInt53
instance decodeJsonMDateTime :: DecodeJson DateTimeWrapper where
      decodeJson json = Right <<< DateTimeWrapper <<< SU.fromJust <<< DJ.toDateTime <<< EU.unsafePerformEffect $ DJ.parse jsonString
          where jsonString :: String
                jsonString = UC.unsafeCoerce json
instance decodeJsonMDate :: DecodeJson DateWrapper where
      decodeJson json = Right <<< DateWrapper <<< SU.fromJust <<< DJ.toDate <<< EU.unsafePerformEffect $ DJ.parse jsonString
          where jsonString :: String
                jsonString = UC.unsafeCoerce json
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

instance ordPrimaryKey :: Ord PrimaryKey where
      compare (PrimaryKey pk) (PrimaryKey anotherPK) = compare pk anotherPK

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