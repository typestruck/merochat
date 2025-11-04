module Server.Profile.Database.Flat where

import Prelude
import Shared.Availability

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Debug (spy)
import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Badge (Badge)
import Shared.DateTime (DateWrapper(..))
import Shared.Privilege (Privilege)
import Shared.Profile.Types (ProfileUser)
import Shared.User (Gender)

type FlatProfileUser =
      { avatared ∷ Maybe String
      , birthday ∷ Maybe Date
      , country ∷ Maybe Int
      , descriptioned ∷ String
      , gender ∷ Maybe Gender
      , onlineStatus ∷ Checked
      , headlined ∷ String
      , privileges ∷ Maybe (Array Privilege)
      , badges ∷ Maybe (Array Badge)
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe (Array Int)
      , named ∷ String
      , tags ∷ Maybe (Array String)
      }

fromFlatProfileUser ∷ FlatProfileUser → ProfileUser
fromFlatProfileUser fu =
      { avatar: fu.avatared
      , age: DateWrapper <$> fu.birthday
      , country: fu.country
      , description: fu.descriptioned
      , gender: fu.gender
      , headline: fu.headlined
      , availability: None
      , onlineStatus: SC.coerce fu.onlineStatus
      , id: fu.id
      , karma: fu.karma
      , karmaPosition: fu.karmaPosition
      , privileges: DM.fromMaybe [] fu.privileges
      , badges: DM.fromMaybe [] fu.badges
      , languages: DM.fromMaybe [] fu.languages
      , name: fu.named
      , tags: DM.fromMaybe [] fu.tags
      }
