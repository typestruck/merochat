module Shared.Types where

import Prelude

import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Date (Date)
import Data.Date as DD
import Data.Either (Either(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.JSDate as DJ
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Effect (Effect)
import Effect.Now as EN
import Effect.Unsafe as EU
import Foreign as F
import Partial.Unsafe as PU

foreign import sss :: forall a. a -> Effect Unit
foreign import fromInt53 :: Int53 -> Json
foreign import toInt53 :: Json -> Int53

-- | Fields for registration or login
newtype RegisterLogin = RegisterLogin {
        email:: String,
        password:: String,
        captchaResponse:: Maybe String
}

derive instance genericRegisterLogin :: Generic RegisterLogin _

-- | tokenPOST is a mitigation for csrf/cookie interception (since httpure http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since PrimaryKey don't to make it a single page application
newtype Token = Token {
        tokenGET :: String,
        tokenPOST :: String
}

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where
        show = DGRS.genericShow

-- | All available endpoints for melanchat
data Route =
        Landing |
        Register |
        Login { next :: Maybe String } |
        IM

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
        show = DGRS.genericShow

-- | Errors that should be reported back to the user
data ResponseError =
        NotFound {
                reason :: String,
                isPost :: Boolean
        } |
        BadRequest { reason :: String } |
        InternalError { reason :: String }

derive instance genericResponseError :: Generic ResponseError _

instance showResponseError :: Show ResponseError where
        show = DGRS.genericShow

data By =
        ID PrimaryKey |
        Email String

newtype PrimaryKey = PrimaryKey Int53

derive instance genericPrimaryKey :: Generic PrimaryKey _
derive instance eqPrimaryKey :: Eq PrimaryKey

instance primaryKeyToSQLValue :: ToSQLValue PrimaryKey where
        toSQLValue (PrimaryKey integer) = F.unsafeToForeign integer

instance primaryKeyFromSQLValue :: FromSQLValue PrimaryKey where
        fromSQLValue = DB.lmap show <<< CME.runExcept <<< map (PrimaryKey <<< DI.fromInt) <<< F.readInt

instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
        encodeJson (PrimaryKey id) = fromInt53 id

instance decodeJsonPrimaryKey :: DecodeJson PrimaryKey where
        decodeJson = Right <<< PrimaryKey <<< toInt53

instance showPrimaryKey :: Show PrimaryKey where
        show = DGRS.genericShow

type BasicUser fields = {
        name :: String,
        headline :: String,
        description :: String,
        gender :: Maybe String |
        fields
}

--fields needed by the IM page
newtype IMUser = IMUser (BasicUser (
        id :: PrimaryKey,
        avatar :: String,
        country :: Maybe String,
        languages :: Array String,
        tags :: Array String,
        age :: Maybe Int
))

derive instance genericIMUser :: Generic IMUser _
derive instance eqIMUser :: Eq IMUser

instance encodeJsonIMUser :: EncodeJson IMUser where
        encodeJson = DAEGR.genericEncodeJson

instance decodeJsonIMUser :: DecodeJson IMUser where
        decodeJson = DADGR.genericDecodeJson

instance showIMUser :: Show IMUser where
        show = DGRS.genericShow

newtype User = User (BasicUser (
        id :: Int53,
        email :: String,
        password :: String,
        recentEmoji :: Maybe String,
        messageOnEnter :: Boolean,
        joined :: Date,
        country :: Maybe Int53,
        birthday :: Maybe Date
))

derive instance genericUser :: Generic User _

--is there not an easier way to do this?
--maybe a approach to select into Row instead of typeclasses for every query?

instance userFromSQLRow :: FromSQLRow User where
        fromSQLRow [
                foreignID,
                foreignName,
                foreignPassword,
                foreignJoined,
                foreignEmail,
                foreignBirthday,
                foreignGender,
                foreignHeadline,
                foreignDescription,
                foreignRecentEmoji,
                foreignCountry,
                foreignMessageOnEnter
        ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- DI.fromInt <$> F.readInt foreignID
                name <- F.readString foreignName
                password <- F.readString foreignPassword
                joined <- PU.unsafePartial (DM.fromJust <<< DJ.toDate) <$> DJ.readDate foreignJoined
                email <- F.readString foreignEmail
                maybeForeignerBirthday <- F.readNull foreignBirthday
                birthday <- DM.maybe (pure Nothing) (map DJ.toDate <<< DJ.readDate) maybeForeignerBirthday
                maybeGender <- F.readNull foreignGender
                gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
                headline <- F.readString foreignHeadline
                description <- F.readString foreignDescription
                maybeRecentEmoji <- F.readNull foreignRecentEmoji
                recentEmoji <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeRecentEmoji
                maybeCountry <- F.readNull foreignCountry
                country <- DM.maybe (pure Nothing) (map (Just <<< DI.fromInt) <<< F.readInt) maybeCountry
                messageOnEnter <- F.readBoolean foreignMessageOnEnter
                pure $ User {
                        id,
                        name,
                        password,
                        joined,
                        email,
                        birthday,
                        gender,
                        headline,
                        description,
                        recentEmoji,
                        country,
                        messageOnEnter
                }
        fromSQLRow _ = Left "missing fields from users table"

-- seems like parsing a postgresql date column fails with a js type error
instance imUserFromSQLRow :: FromSQLRow IMUser where
        fromSQLRow [
                foreignID,
                foreignGender,
                foreignBirthday,
                foreignName,
                foreignHeadline,
                foreignDescription,
                foreignCountry,
                foreignLanguages,
                foreignTags
        ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- PrimaryKey <<< DI.fromInt <$> F.readInt foreignID
                name <- F.readString foreignName
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
                let now = EU.unsafePerformEffect $ EN.nowDate
                pure $ IMUser {
                        id,
                        name,
                        --this is a bug.......
                        age: map ((DE.fromEnum (DD.year now) - _) <<< DE.fromEnum <<< DD.year) birthday,
                        gender,
                        headline,
                        description,
                        country,
                        languages,
                        tags,
                        avatar:"/client/media/avatar.png"
                }
        fromSQLRow _ = Left "missing fields from users table"


newtype IMModel = IMModel {
        user :: IMUser,
        suggestions :: Array IMUser,
        chatting :: Maybe Int
}

derive instance genericIMModel :: Generic IMModel _
derive instance eqIMModel :: Eq IMModel

instance showIMModel :: Show IMModel where
        show = DGRS.genericShow

data SuggestionMessage =
        NextSuggestion

data IMMessage =
        SM SuggestionMessage
