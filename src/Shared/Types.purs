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
import Data.Int53 as DI
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

foreign import fromInt53 :: Int53 -> Json
foreign import toInt53 :: Json -> Int53

type BasicUser fields = {
        id :: PrimaryKey,
        name :: String,
        headline :: String,
        description :: String,
        gender :: Maybe String |
        fields
}

newtype User = User (BasicUser (
        email :: String,
        password :: String,
        recentEmoji :: Maybe String,
        messageOnEnter :: Boolean,
        joined :: Date,
        country :: Maybe Int53,
        birthday :: Maybe Date
))

newtype PrimaryKey = PrimaryKey Int53

-- | Fields for registration or login
newtype RegisterLogin = RegisterLogin {
        email:: String,
        password:: String,
        captchaResponse:: Maybe String
}

-- | tokenPOST is a mitigation for csrf/cookie interception (since httpure http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests
newtype Token = Token {
        tokenGET :: String,
        tokenPOST :: String
}

-- | All available endpoints for melanchat
data Route =
        Landing |
        Register |
        Login { next :: Maybe String } |
        IM

data By =
        ID PrimaryKey |
        Email String

-- | Errors that should be reported back to the user
data ResponseError =
        NotFound {
                reason :: String,
                isPost :: Boolean
        } |
        BadRequest { reason :: String } |
        InternalError { reason :: String }

derive instance genericRegisterLogin :: Generic RegisterLogin _
derive instance genericRoute :: Generic Route _
derive instance genericToken :: Generic Token _
derive instance genericResponseError :: Generic ResponseError _
derive instance genericPrimaryKey :: Generic PrimaryKey _
derive instance genericUser :: Generic User _

derive instance eqRoute :: Eq Route
derive instance eqPrimaryKey :: Eq PrimaryKey

instance showToken :: Show Token where
        show = DGRS.genericShow
instance showRoute :: Show Route where
        show = DGRS.genericShow
instance showResponseError :: Show ResponseError where
        show = DGRS.genericShow
instance showPrimaryKey :: Show PrimaryKey where
        show = DGRS.genericShow

instance primaryKeySemiring :: Semiring PrimaryKey where
        add (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a + b)
        zero = PrimaryKey $ DI.fromInt 0
        mul (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a * b)
        one = PrimaryKey $ DI.fromInt 1

instance hashablePrimaryKey :: Hashable PrimaryKey where
        hash (PrimaryKey key) = DH.hash $ DI.toNumber key

instance primaryKeyToSQLValue :: ToSQLValue PrimaryKey where
        toSQLValue (PrimaryKey integer) = F.unsafeToForeign integer

instance primaryKeyFromSQLValue :: FromSQLValue PrimaryKey where
        fromSQLValue = DB.lmap show <<< CME.runExcept <<< parsePrimaryKey

parsePrimaryKey :: _ -> _
parsePrimaryKey data_
        | F.typeOf data_ == "number" = map (PrimaryKey <<< SU.unsafeFromJust "parsePrimaryKey" <<< DI.fromNumber) $ F.readNumber data_
        | otherwise = map (PrimaryKey <<< SU.unsafeFromJust "parsePrimaryKey" <<< DI.fromString) $ F.readString data_

instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
        encodeJson (PrimaryKey id) = fromInt53 id

instance decodeJsonPrimaryKey :: DecodeJson PrimaryKey where
        decodeJson = Right <<< PrimaryKey <<< toInt53

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance userFromSQLRow :: FromSQLRow User where
        fromSQLRow [
                foreignID,
                foreignUnread,
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
                id <- parsePrimaryKey foreignID
                name <- F.readString foreignUnread
                password <- F.readString foreignPassword
                joined <- SU.unsafeFromJust "userFromSQLRow" <<< DJ.toDate <$> DJ.readDate foreignJoined
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
