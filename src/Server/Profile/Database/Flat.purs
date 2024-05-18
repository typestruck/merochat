module Server.Profile.Database.Flat where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Debug (spy)
import Shared.Avatar as SA
import Shared.DateTime (DateWrapper(..))
import Shared.Privilege (Privilege)
import Shared.Profile.Types (ProfileUser)
import Shared.User (Gender)
import Shared.Availability

type FlatProfileUser =
      { avatar ∷ Maybe String
      , birthday ∷ Maybe Date
      , country ∷ Maybe Int
      , description ∷ String
      , gender ∷ Maybe Gender
      , headline ∷ String
      , privileges ∷ Maybe (Array Privilege)
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe (Array Int)
      , name ∷ String
      , tags ∷ Maybe (Array String)
      }

fromFlatProfileUser ∷ FlatProfileUser → ProfileUser
fromFlatProfileUser fu =
      { avatar: SA.parseAvatar fu.avatar
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
      , languages: DM.fromMaybe [] fu.languages
      , name: fu.name
      , tags: DM.fromMaybe [] fu.tags
      }
