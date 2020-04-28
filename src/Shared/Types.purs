module Shared.Types where

import Prelude

import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as DGRS
import Data.Hashable (class Hashable)
import Data.Hashable as DH
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe)
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Foreign (Foreign, F)
import Foreign as F
import Shared.Unsafe as SU

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

newtype RegisterLoginUser = RegisterLoginUser {
        id :: PrimaryKey,
        email :: String,
        password :: String
}

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

-- | A newtype for pure string JSON payloads so we can use the same `Generic` functions
newtype JSONString = JSONString String

-- | Used by requests which don't meaningfully respond anything
data Ok = Ok

-- | All available endpoints for melanchat
data Route =
        Landing |
        Register |
        Login { next :: Maybe String } |
        IM |
        Profile

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

derive instance genericOk :: Generic Ok _
derive instance genericRegisterLogin :: Generic RegisterLogin _
derive instance genericRoute :: Generic Route _
derive instance genericToken :: Generic Token _
derive instance genericResponseError :: Generic ResponseError _
derive instance genericPrimaryKey :: Generic PrimaryKey _
derive instance genericUser :: Generic RegisterLoginUser _
derive instance genericJSONString :: Generic JSONString _

derive instance eqOk :: Eq Ok
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
instance showOk :: Show Ok where
        show = DGRS.genericShow

instance primaryKeySemiring :: Semiring PrimaryKey where
        add (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a + b)
        zero = PrimaryKey $ DI.fromInt 0
        mul (PrimaryKey a) (PrimaryKey b) = PrimaryKey (a * b)
        one = PrimaryKey $ DI.fromInt 1

instance hashablePrimaryKey :: Hashable PrimaryKey where
        hash (PrimaryKey key) = DH.hash $ DI.toNumber key

instance primaryKeyToSQLValue :: ToSQLValue PrimaryKey where
        toSQLValue (PrimaryKey int53) = F.unsafeToForeign $ DI.toNumber int53

instance primaryKeyFromSQLValue :: FromSQLValue PrimaryKey where
        fromSQLValue = DB.lmap show <<< CME.runExcept <<< parsePrimaryKey

parsePrimaryKey :: Foreign -> F PrimaryKey
parsePrimaryKey data_
        | F.typeOf data_ == "number" = map (PrimaryKey <<< SU.unsafeFromJust "parsePrimaryKey" <<< DI.fromNumber) $ F.readNumber data_
        | otherwise = map (PrimaryKey <<< SU.unsafeFromJust "parsePrimaryKey" <<< DI.fromString) $ F.readString data_

instance encodeJsonOk :: EncodeJson Ok where
        encodeJson = DAEGR.genericEncodeJson
instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
        encodeJson (PrimaryKey id) = fromInt53 id

instance decodeJsonOk :: DecodeJson Ok where
        decodeJson = DADGR.genericDecodeJson
instance decodeJsonPrimaryKey :: DecodeJson PrimaryKey where
        decodeJson = Right <<< PrimaryKey <<< toInt53

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

instance fromSQLRowResiterLoginUser :: FromSQLRow RegisterLoginUser where
        fromSQLRow [foreignID, foreignEmail, foreignPassword] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- parsePrimaryKey foreignID
                email <- F.readString foreignEmail
                password <- F.readString foreignPassword
                pure $ RegisterLoginUser { id, email, password }
        fromSQLRow _ = Left "missing/extra fields from users table"

instance ordPrimaryKey :: Ord PrimaryKey where
        compare (PrimaryKey pk) (PrimaryKey anotherPK) = compare pk anotherPK