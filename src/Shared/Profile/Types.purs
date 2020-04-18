module Shared.Profile.Types where

import Prelude
import Shared.Types

import Control.Monad.Except as CME
import Data.Bifunctor as DB
import Data.DateTime (Date)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.JSDate as DJ
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Database.PostgreSQL (class FromSQLRow)
import Foreign as F
import Shared.IM.Types (MDate(..))

newtype ProfileUser = ProfileUser (BasicUser (
        avatar :: String,
        country :: Maybe String,
        languages :: Array String,
        tags :: Array String,
        age :: Maybe MDate
))

data ProfileMessage = ProfileMessage

derive instance genericProfileUser :: Generic ProfileUser _

instance profileUserFromSQLRow :: FromSQLRow ProfileUser where
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
                pure $ ProfileUser {
                        id,
                        name,
                        age: MDate <$> birthday,
                        gender,
                        headline,
                        description,
                        country,
                        languages,
                        tags,
                        avatar: "/client/media/avatar.png"
                }
        fromSQLRow _ = Left "missing or extra fields from users table"