module Shared.Profile.Types where

import Prelude
import Shared.Types

import Control.Monad.Except as CME
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.JSDate as DJ
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol (SProxy(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (class FromSQLRow)
import Flame (Key)
import Foreign as F
import Prim.Row (class Cons)
import Shared.IM.Types (MDate(..))

newtype ProfileModel = ProfileModel {
        user :: ProfileUser
}

type A = (BasicUser (
        avatar :: String,
        country :: Maybe String,
        languages :: Array String,
        tags :: Array String,
        age :: Maybe MDate
))

newtype ProfileUser = ProfileUser A

--REFACTOR: write a generic SetField message
--REFACTOR: write a generic Enter message
data ProfileMessage =
        SelectAvatar |
        SetAvatar String |
        SetName String |
        NameEnter (Tuple Key String) |
        SetHeadline String |
        HeadlineEnter (Tuple Key String) |
        SaveProfile

derive instance genericProfileModel :: Generic ProfileModel _
derive instance genericProfileUser :: Generic ProfileUser _

derive instance newtypeProfileModel :: Newtype ProfileModel _
derive instance newtypeProfileUser :: Newtype ProfileUser _

instance encodeJsonProfileUser :: EncodeJson ProfileUser where
        encodeJson = DAEGR.genericEncodeJson

instance decodeJsonProfileUser :: DecodeJson ProfileUser where
        decodeJson = DADGR.genericDecodeJson

instance fromSQLRowProfileUser :: FromSQLRow ProfileUser where
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
                foreignTags
        ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
                id <- parsePrimaryKey foreignID
                maybeForeignerAvatar <- F.readNull foreignAvatar
                --REFACTOR: all image paths
                avatar <- DM.maybe (pure "/client/media/avatar.png") (map ("/client/media/upload/" <> _ ) <<< F.readString) maybeForeignerAvatar
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
                pure $ ProfileUser {
                        id,
                        avatar,
                        name,
                        age: MDate <$> birthday,
                        gender,
                        headline,
                        description,
                        country,
                        languages,
                        tags
                }
        fromSQLRow _ = Left "missing or extra fields from users table"