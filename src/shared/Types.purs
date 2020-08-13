module Shared.Types where

import Prelude

import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.DateTime (Date, DateTime(..), Time(..))
import Data.Either (Either(..))
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
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Database.PostgreSQL (class FromSQLRow, class ToSQLValue, class FromSQLValue)
import Effect.Unsafe as EU
import Foreign (Foreign, F)
import Foreign as F
import Shared.Unsafe as SU
import Unsafe.Coerce as UC

foreign import data Editor :: Type
foreign import data Trie :: Type

foreign import fromEditor :: Editor -> Json
foreign import fromJSDate :: JSDate -> Json
foreign import fromInt53 :: Int53 -> Json
foreign import toEditor :: Json -> Editor
foreign import toInt53 :: Json -> Int53

type BasicUser fields = {
      id :: PrimaryKey,
      name :: String,
      headline :: String,
      description :: String |
      fields
}

newtype RegisterLoginUser = RegisterLoginUser {
      id :: PrimaryKey,
      email :: String,
      password :: String
}

newtype MDateTime = MDateTime DateTime

newtype MDate = MDate Date

newtype PrimaryKey = PrimaryKey Int53

type EmailCaptcha r = {
      email:: String,
      captchaResponse:: Maybe String |
      r
}

-- | Fields for registration or login
newtype RegisterLogin = RegisterLogin (EmailCaptcha (password :: String))

newtype RecoverAccount = RecoverAccount (EmailCaptcha ())

newtype ResetPassword = ResetPassword {
      token :: String,
      password :: String
}

-- | tokenPOST is a mitigation for csrf/cookie interception (since httpure http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests
newtype Token = Token {
      tokenGET :: String,
      tokenPOST :: String
}

--these wrappers are for json type safety
newtype SettingsPayload = SettingsPayload String
newtype GeneratePayload = GeneratePayload String

-- | Used by requests which don't meaningfully respond anything
data Ok = Ok

data Gender =
      Female |
      Male |
      NonBinary |
      Other

-- | All available endpoints for melanchat
data Route =
      Landing |
      Register |
      Login { next :: Maybe String } |
      IM |
      Profile |
      Generate { what :: Generate} |
      Settings |
      AccountEmail |
      AccountPassword |
      Terminate |
      Suggestions |
      Reset |
      Recover { token :: Maybe String } |
      SingleContact { id :: PrimaryKey } |
      Contacts { skip :: Int } |
      History { skip :: Int, with :: PrimaryKey } |
      Block { id :: PrimaryKey }

data Generate =
      Name |
      Headline |
      Description

data By =
      ID PrimaryKey |
      Email String

-- | Errors that should be reported back to the user
data ResponseError =
      NotFound {
            reason :: String,
            isPost :: Boolean
      } |
      LoginRequired {
            next :: String,
            isPost :: Boolean
      } |
      AnonymousRequired |
      BadRequest { reason :: String } |
      InternalError { reason :: String }

derive instance newtypeMDateTime :: Newtype MDateTime _
derive instance newtypePrimaryKey :: Newtype PrimaryKey _

derive instance genericGeneratePayload :: Generic GeneratePayload _
derive instance genericSettingsPayload :: Generic SettingsPayload _
derive instance genericResetPassword :: Generic ResetPassword _
derive instance genericOk :: Generic Ok _
derive instance genericRecover :: Generic RecoverAccount _
derive instance genericGenerate :: Generic Generate _
derive instance genericGender :: Generic Gender _
derive instance genericRegisterLogin :: Generic RegisterLogin _
derive instance genericRoute :: Generic Route _
derive instance genericToken :: Generic Token _
derive instance genericResponseError :: Generic ResponseError _
derive instance genericPrimaryKey :: Generic PrimaryKey _
derive instance genericUser :: Generic RegisterLoginUser _
derive instance genericMDateTime :: Generic MDateTime _
derive instance genericMDate :: Generic MDate _

derive instance eqGenerate :: Eq Generate
derive instance eqRecover :: Eq RecoverAccount
derive instance eqMDateTime :: Eq MDateTime
derive instance eqMDate :: Eq MDate
derive instance eqOk :: Eq Ok
derive instance eqGender :: Eq Gender
derive instance eqRoute :: Eq Route
derive instance eqPrimaryKey :: Eq PrimaryKey

instance showGenerate :: Show Generate where
      show = DGRS.genericShow
instance showToken :: Show Token where
      show = DGRS.genericShow
instance showRoute :: Show Route where
      show = DGRS.genericShow
instance showResponseError :: Show ResponseError where
      show = DGRS.genericShow
instance showPrimaryKey :: Show PrimaryKey where
      show (PrimaryKey i) = DS.replace (Pattern ".0") (Replacement "") <<< show $ DI.toNumber i
instance showOk :: Show Ok where
      show = DGRS.genericShow
instance showGender :: Show Gender where
      show Female = "Female"
      show Male = "Male"
      show NonBinary = "Non binary"
      show Other = "Other"
instance showMDateTime :: Show MDateTime where
      show = DGRS.genericShow
instance showMDate :: Show MDate where
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

instance encodeJsonResetPassword :: EncodeJson ResetPassword where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonEditor :: EncodeJson Editor where
      encodeJson editor = fromEditor editor
instance encodeJsonGender :: EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonOk :: EncodeJson Ok where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonRecover :: EncodeJson RecoverAccount where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
      encodeJson (PrimaryKey id) = fromInt53 id
instance encodeJsonMDateTime :: EncodeJson MDateTime where
      encodeJson (MDateTime dateTime) = fromJSDate $ DJ.fromDateTime dateTime
instance encodeJsonMDate :: EncodeJson MDate where
      encodeJson (MDate date) = fromJSDate <<< DJ.fromDateTime <<< DateTime date $ Time (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0) (SU.fromJust $ DE.toEnum 0)

instance decodeJsonResetPassword :: DecodeJson ResetPassword where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonEditor :: DecodeJson Editor where
      decodeJson = Right <<< toEditor
instance decodeJsonGender :: DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonOk :: DecodeJson Ok where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonRecover :: DecodeJson RecoverAccount where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonPrimaryKey :: DecodeJson PrimaryKey where
      decodeJson = Right <<< PrimaryKey <<< toInt53
instance decodeJsonMDateTime :: DecodeJson MDateTime where
      decodeJson json = Right <<< MDateTime <<< SU.fromJust <<< DJ.toDateTime <<< EU.unsafePerformEffect $ DJ.parse jsonString
            where jsonString :: String
                  jsonString = UC.unsafeCoerce json
instance decodeJsonMDate :: DecodeJson MDate where
      decodeJson json = Right <<< MDate <<< SU.fromJust <<< DJ.toDate <<< EU.unsafePerformEffect $ DJ.parse jsonString
            where jsonString :: String
                  jsonString = UC.unsafeCoerce json

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