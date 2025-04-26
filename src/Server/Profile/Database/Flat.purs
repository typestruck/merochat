module Server.Profile.Database.Flat where

import Prelude
import Shared.Availability

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Debug (spy)
import Shared.Avatar as SA
import Shared.Badge (Badge)
import Shared.DateTime (DateWrapper(..))
import Shared.Privilege (Privilege)
import Shared.Profile.Types (ProfileUser)
import Shared.User (Gender)

type FlatProfileUser =
      { avatar ∷ Maybe String
      , birthday ∷ Maybe Date
      , country ∷ Maybe Int
      , description ∷ String
      , gender ∷ Maybe Gender
      , headline ∷ String
      , privileges ∷ Maybe (Array Privilege)
      , badges ∷ Maybe (Array Badge)
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe (Array Int)
      , name ∷ String
      , tags ∷ Maybe (Array String)
      }

fromFlatProfileUser ∷ FlatProfileUser → ProfileUser
fromFlatProfileUser fu =
      { avatar: fu.avatar
      , age: DateWrapper <$> fu.birthday
      , country: fu.country
      , description: fu.description
      , gender: fu.gender
      , headline: fu.headline
      , availability: None
      , id: fu.id
      , karma: fu.karma
      , karmaPosition: fu.karmaPosition
      , privileges: DM.fromMaybe [] fu.privileges
      , badges: DM.fromMaybe [] fu.badges
      , languages: DM.fromMaybe [] fu.languages
      , name: fu.name
      , tags: DM.fromMaybe [] fu.tags
      }
