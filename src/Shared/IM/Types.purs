module Shared.IM.Types where

import Prelude

import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Date (Date)
import Shared.Unsafe as SU
import Data.Array as DA
import Data.Date as DD
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.Hashable (class Hashable)
import Data.Hashable as DH
import Data.Int53 (Int53)
import Data.Int as DIN
import Data.JSDate as DJ
import Data.JSDate (JSDate)
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Unsafe.Coerce as UC
import Data.String (Pattern(..))
import Data.String as DS
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Database.PostgreSQL as DP
import Effect.Now as EN
import Effect.Unsafe as EU
import Foreign as F
import Partial.Unsafe as PU
import Web.Socket.WebSocket (WebSocket)
import Data.Date as DD
import Shared.Types(parsePrimaryKey, PrimaryKey(..))
import Data.Time.Duration(Days(..))

foreign import fromJSDate :: JSDate -> Json
foreign import fromWS :: WebSocket -> Json
foreign import toWS :: Json -> WS
foreign import eqWS :: WebSocket -> WebSocket -> Boolean

type BasicUser fields = {
        id :: PrimaryKey,
        name :: String,
        headline :: String,
        description :: String,
        gender :: Maybe String |
        fields
}

type BasicMessage fields = {
        id :: PrimaryKey,
        content :: String |
        fields
}

newtype WS = WS WebSocket

--fields needed by the IM page
newtype IMUser = IMUser (BasicUser (
        avatar :: String,
        country :: Maybe String,
        languages :: Array String,
        tags :: Array String,
        age :: Maybe Int,
        message :: String,
        history :: Array HistoryMessage
))

newtype IMModel = IMModel {
        user :: IMUser,
        suggestions :: Array IMUser,
        suggesting :: Maybe Int,
        contacts :: Array IMUser,
        chatting :: Maybe Int,
        webSocket :: Maybe WS,
        temporaryID :: PrimaryKey,
        token :: Maybe String
}

newtype HistoryMessage = HistoryMessage {
        id :: PrimaryKey,
        sender :: PrimaryKey,
        recipient :: PrimaryKey,
        date :: MDateTime,
        content :: String,
        status :: MessageStatus
}

data MessageStatus = Unread | Read

newtype MDateTime = MDateTime DateTime

data ContactMessage =
        ResumeChat PrimaryKey

data SuggestionMessage =
        NextSuggestion

data ChatMessage =
        SendMessage String |
        ReceiveMessage WebSocketPayloadClient

data MainMessage =
        SetWebSocket WebSocket |
        SetToken String

data IMMessage =
        SM SuggestionMessage |
        CM ChatMessage |
        MM MainMessage |
        CNM ContactMessage

data WebSocketPayloadServer =
        Connect String |
        ServerMessage (BasicMessage (
                token :: String,
                user :: PrimaryKey
        )) |
        ReadMessages {
                token :: String,
                --alternatively, update by user?
                ids :: Array PrimaryKey
        }

data WebSocketPayloadClient =
        ClientMessage (BasicMessage (
                user :: Either IMUser PrimaryKey,
                date :: MDateTime
        ))|
        Received {
                previousID :: PrimaryKey,
                id :: PrimaryKey
        }

derive instance genericIMUser :: Generic IMUser _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericWS :: Generic WS _
derive instance genericIMModel :: Generic IMModel _
derive instance genericHistoryMessage :: Generic HistoryMessage _
derive instance genericMessageStatus :: Generic MessageStatus _
derive instance genericMDateTime :: Generic MDateTime _

derive instance newTypeIMUser :: Newtype IMUser _
derive instance newTypeHistoryMessage :: Newtype HistoryMessage _
derive instance newTypeIMModel :: Newtype IMModel _

derive instance eqMDateTime :: Eq MDateTime
derive instance eqHistoryMessage :: Eq HistoryMessage
derive instance eqIMModel :: Eq IMModel
derive instance eqIMUser :: Eq IMUser
derive instance eqMessageStatus :: Eq MessageStatus
instance eqWSW :: Eq WS where
        eq (WS w) (WS s) = eqWS w s

instance showHistoryMessage :: Show HistoryMessage where
        show = DGRS.genericShow
instance showIMUser :: Show IMUser where
        show = DGRS.genericShow
instance showWebSocketPayloadClient :: Show WebSocketPayloadClient where
        show = DGRS.genericShow
instance showWebSocketPayloadServer :: Show WebSocketPayloadServer where
        show = DGRS.genericShow
instance showWS :: Show WS where
        show _ = "web socket"
instance showIMModel :: Show IMModel where
        show = DGRS.genericShow
instance showMessageStatus :: Show MessageStatus where
        show = DGRS.genericShow

instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
        encodeJson = DAEGR.genericEncodeJson
instance encodeJsonWS :: EncodeJson WS where
        encodeJson (WS ws) = fromWS ws
instance encodeJsonIMUser :: EncodeJson IMUser where
        encodeJson = DAEGR.genericEncodeJson
instance encodeJsonHistoryMessage :: EncodeJson HistoryMessage where
        encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMDateTime :: EncodeJson MDateTime where
        encodeJson (MDateTime dateTime) = fromJSDate $ DJ.fromDateTime dateTime

instance decodeJsonHistoryMessage :: DecodeJson HistoryMessage where
        decodeJson = DADGR.genericDecodeJson
instance decodeJsonMessageStatus :: DecodeJson MessageStatus where
        decodeJson = DADGR.genericDecodeJson
instance decodeJsonWS :: DecodeJson WS where
        decodeJson = Right <<< toWS
instance decodeJsonIMUser :: DecodeJson IMUser where
        decodeJson = DADGR.genericDecodeJson
instance showMDateTime :: Show MDateTime where
        show = DGRS.genericShow
instance edecodeJsonMDateTime :: DecodeJson MDateTime where
        decodeJson json = Right <<< MDateTime <<< SU.unsafeFromJust "decodeJson mdatetime" <<< DJ.toDateTime <<< EU.unsafePerformEffect $ DJ.parse jsonString
                where   jsonString :: String
                        jsonString = UC.unsafeCoerce json

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance imUserFromSQLRow :: FromSQLRow IMUser where
        fromSQLRow [
                foreignID,
                foreignGender,
                foreignBirthday,
                foreignUnread,
                foreignHeadline,
                foreignDescription,
                foreignCountry,
                foreignLanguages,
                foreignTags
        ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- parsePrimaryKey foreignID
                name <- F.readString foreignUnread
                maybeForeignerBirthday <- F.readNull foreignBirthday
                birthday <- DM.maybe (pure Nothing) (map DJ.toDate <<< DJ.readDate) maybeForeignerBirthday
                maybeGender <- F.readNull foreignGender
                gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
                headline <- F.readString foreignHeadline
                description <- F.readString foreignDescription
                maybeCountry <- F.readNull foreignCountry
                country <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeCountry
                maybeLanguages <- F.readNull foreignLanguages
                languages <- DM.maybe (pure []) (map (DS.split (Pattern ",")) <<< F.readString) maybeLanguages
                maybeTags <- F.readNull foreignTags
                tags <- DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags
                let     now = EU.unsafePerformEffect EN.nowDate
                pure $ IMUser {
                        id,
                        name,
                        --this will yield a wrong result in some cases, but I guess it is fair for a ASL field
                        age: (\(Days d) -> DIN.ceil (d / 365.0)) <<< DD.diff now <$> birthday,
                        gender,
                        headline,
                        description,
                        country,
                        languages,
                        tags,
                        message: "",
                        history: [],
                        avatar: "/client/media/avatar.png"
                }
        --this is surely not ideal
        fromSQLRow list@[
                foreignDate, -- there is an extra field needed by the distinct when select imusers for the contact list
                foreignID,
                foreignGender,
                foreignBirthday,
                foreignUnread,
                foreignHeadline,
                foreignDescription,
                foreignCountry,
                foreignLanguages,
                foreignTags
        ] = DP.fromSQLRow <<< SU.unsafeFromJust  "fromSQLRow" $ DA.tail list :: Either String IMUser
        fromSQLRow _ = Left "missing or extra fields from users table"

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
                date <- MDateTime <<< SU.unsafeFromJust "fromSQLRow" <<< DJ.toDateTime <$> DJ.readDate foreignDate
                content <- F.readString foreignContent
                status <- SU.unsafeFromJust "fromSQLRow" <<< DE.toEnum <$> F.readInt foreignStatus
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