module Shared.Types where

import Prelude

import Control.Monad.Except as CME
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor as DB
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as S
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.JSDate as DJ
import Database.PostgreSQL (class ToSQLValue, Pool, class FromSQLValue)
import Foreign as F
import Data.List.NonEmpty as DLN
import Control.Monad.Except as CME
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Database.PostgreSQL (class FromSQLRow)
import Foreign as F
import Partial.Unsafe as PU

foreign import fromDate :: Date -> Json
foreign import fromInt53 :: Int53 -> Json
foreign import fromIMUser :: SanitizedUser () -> Json

-- | Fields for registration or login
newtype RegisterLogin = RegisterLogin {
        email:: String,
        password:: String,
        captchaResponse:: Maybe String
}

derive instance genericRegisterLogin :: Generic RegisterLogin _

-- | tokenPOST is a mitigation for csrf/cookie interception (since httpure http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application
newtype Token = Token {
        tokenGET :: String,
        tokenPOST :: String
}

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where
        show = S.genericShow

-- | All available endpoints for melanchat
data Route =
        Landing |
        Register |
        Login { next :: Maybe String } |
        IM

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
        show = S.genericShow

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
        show = S.genericShow

newtype GDate = GDate Date

derive instance genericDate :: Generic GDate _

instance encodeJsonGdate :: EncodeJson GDate where
        encodeJson (GDate date) = fromDate date

instance encodeJsonPrimaryKey :: EncodeJson PrimaryKey where
        encodeJson (PrimaryKey id) = fromInt53 id

data By = ID PrimaryKey | Email String

newtype PrimaryKey = PrimaryKey Int53

instance primaryKeyToSQLValue :: ToSQLValue PrimaryKey where
        toSQLValue (PrimaryKey integer) = F.unsafeToForeign integer

instance primaryKeyFromSQLValue :: FromSQLValue PrimaryKey where
        fromSQLValue = DB.lmap show <<< CME.runExcept <<< map (PrimaryKey <<< DI.fromInt) <<< F.readInt

type SanitizedUser fields = {
        id :: PrimaryKey,
        name :: String,
        email :: String,
        joined :: GDate,
        headline :: String,
        description :: String,
        birthday :: Maybe GDate,
        gender :: Maybe String,
        recentEmoji :: Maybe String,
        country :: Maybe PrimaryKey,
        messageOnEnter :: Boolean |
        fields
}

--fields needed by the IM page
newtype IMUser = IMUser (SanitizedUser ())

derive instance genericIMUser :: Generic IMUser _

instance encodeIMUser :: EncodeJson IMUser where
        encodeJson (IMUser user) = fromIMUser user

newtype User = User (SanitizedUser (password :: String))

derive instance genericUser :: Generic User _

instance userFromSQLRow :: FromSQLRow User where
        fromSQLRow [
                foreignerID,
                foreignerName,
                foreignerPassword,
                foreignerJoined,
                foreignerEmail,
                foreignerBirthday,
                foreignerGender,
                foreignerHeadline,
                foreignerDescription,
                foreignerRecentEmoji,
                foreignerCountry,
                foreignerMessageOnEnter
        ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- DI.fromInt <$> F.readInt foreignerID
                name <- F.readString foreignerName
                password <- F.readString foreignerPassword
                joined <- PU.unsafePartial (DM.fromJust <<< DJ.toDate) <$> DJ.readDate foreignerJoined
                email <- F.readString foreignerEmail
                maybeForeignerBirthday <- F.readNull foreignerBirthday
                birthday <- DM.maybe (pure Nothing) (map DJ.toDate <<< DJ.readDate) maybeForeignerBirthday
                maybeGender <- F.readNull foreignerGender
                gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
                headline <- F.readString foreignerHeadline
                description <- F.readString foreignerDescription
                maybeRecentEmoji <- F.readNull foreignerRecentEmoji
                recentEmoji <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeRecentEmoji
                maybeCountry <- F.readNull foreignerCountry
                country <- DM.maybe (pure Nothing) (map (Just <<< DI.fromInt) <<< F.readInt) maybeCountry
                messageOnEnter <- F.readBoolean foreignerMessageOnEnter
                pure $ User {
                        id: PrimaryKey id,
                        name,
                        password,
                        joined: GDate joined,
                        email,
                        birthday: map GDate birthday,
                        gender,
                        headline,
                        description,
                        recentEmoji,
                        country: map PrimaryKey country,
                        messageOnEnter
                }
        fromSQLRow _ = Left "missing fields from users table"

newtype IMModel = IMModel {
        user :: IMUser
}

derive instance genericIMModel :: Generic IMModel _

data IMMessage